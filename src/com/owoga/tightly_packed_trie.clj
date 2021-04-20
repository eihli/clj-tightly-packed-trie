(ns com.owoga.tightly-packed-trie
  (:require [com.owoga.trie :as trie]
            [com.owoga.tightly-packed-trie.encoding :as encoding]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [com.owoga.tightly-packed-trie.bit-manip :as bm])
  (:import (java.io ByteArrayOutputStream ByteArrayInputStream
                    DataOutputStream DataInputStream)))

#_(set! *warn-on-reflection* true)

;; A trie data structure that can be converted to
;; a contiguous array of bytes while maintaining
;; efficient lookups.
;;
;; A regular Clojure hash-map can be used as a trie,
;; but there's a lot of memory overhead that comes along
;; with hash-maps.
;;
;; To conveniently get the benefit of packing a trie into a contiguous array of
;; bytes, there are a few restrictions.
;;
;; Everything must be numeric IDs. Keys must be numeric IDs.
;; Values must be numeric IDs. Everything must be a number.
;;
;; This lets us encode everything as variable-length-encoded byte arrays.
;;
;; To maximize efficiency, your most common keys should have the
;; smallest IDs.

(defmacro wrap-byte-buffer
  "Saves the position and limit of a byte buffer, runs body,
  returns byte buffer to original position and limit."
  [byte-buffer & body]
  `(let [original-position# (.position ~byte-buffer)
         original-limit# (.limit ~byte-buffer)]
     (try (do ~@body)
          (finally
            (.limit ~byte-buffer original-limit#)
            (.position ~byte-buffer original-position#)))))

(defn rewind-to-key [^java.nio.ByteBuffer bb
                     ^Integer stop]
  (loop []
    (let [current (.get bb (.position bb))
          previous (.get bb (dec (.position bb)))]
      (if (or (= stop (.position bb))
              (and (encoding/key-byte? current)
                   (encoding/offset-byte? previous)))
        bb
        (do (.position bb (dec (.position bb)))
            (recur))))))

(defn find-key-in-index
  [^java.nio.ByteBuffer bb
   ^Integer target-key
   ^Integer max-address
   not-found]
  (.limit bb max-address)
  (let [key
        (loop [previous-key nil
               min-position (.position bb)
               max-position max-address]
          (if (zero? (- max-position min-position))
            not-found
            (let [^Integer mid-position (+ min-position (quot (- max-position min-position) 2))]
              (.position bb mid-position)
              (let [^java.nio.ByteBuffer bb (rewind-to-key bb min-position)
                    current-key
                    (encoding/decode-number-from-tightly-packed-trie-index bb)]
                (cond
                  (= current-key target-key)
                  (encoding/decode-number-from-tightly-packed-trie-index bb)

                  (= current-key previous-key)
                  (do
                    (encoding/decode-number-from-tightly-packed-trie-index bb)
                    (let [final-key (encoding/decode-number-from-tightly-packed-trie-index bb)]
                      (if (= target-key final-key)
                        (encoding/decode-number-from-tightly-packed-trie-index bb)
                        (throw (Exception. "Key not found.")))))

                  (< current-key target-key)
                  ;; Chew the next decoded number. It's a useless offset.
                  (do
                    (encoding/decode-number-from-tightly-packed-trie-index bb)
                    (recur
                     current-key
                     (.position bb)
                     max-position))

                  (> current-key target-key)
                  ;; This could also be rewound.
                  (do
                    (rewind-to-key bb min-position)
                    (recur
                     current-key
                     min-position
                     (.position bb))))))))]
    (.limit bb (.capacity bb))
    key))

(defn tightly-packed-trie-node-value
  [byte-buffer]
  (let [value (encoding/decode byte-buffer)
        freq (encoding/decode byte-buffer)]
    {:id value
     :count freq}))

(declare -value)
(declare children-memo)

(deftype TightlyPackedTrie [^java.nio.ByteBuffer byte-buffer
                            ^Integer key
                            ^Integer address
                            ^Integer limit
                            value-decode-fn]
  trie/ITrie
  (lookup [self ks]
    (wrap-byte-buffer
     byte-buffer
     (.limit byte-buffer limit)
     (.position byte-buffer address)
     (if (empty? ks)
       self
       (let [val (value-decode-fn byte-buffer)
             size-of-index (encoding/decode byte-buffer)
             offset (find-key-in-index
                     byte-buffer
                     (first ks)
                     (+ (.position byte-buffer) size-of-index)
                     :not-found)]
         (if (= offset :not-found)
           nil
           (let [child (TightlyPackedTrie.
                        byte-buffer
                        (first ks)
                        (- address offset)
                        (.capacity byte-buffer)
                        value-decode-fn)]
             (trie/lookup child (rest ks))))))))
  (children [self]
    (children- byte-buffer address limit value-decode-fn))

  clojure.lang.ILookup
  (valAt [self ks]
    (if-let [^TightlyPackedTrie node (trie/lookup self ks)]
      (-value node value-decode-fn)
      nil))
  (valAt [self ks not-found]
    (or (get self ks) not-found))

  clojure.lang.Counted
  (count [trie]
    (count (seq trie)))

  clojure.lang.Seqable
  (seq [trie]
    (let [step (fn step [^clojure.lang.PersistentList path
                         [[^TightlyPackedTrie node & nodes] & stack]
                         [^TightlyPackedTrie parent & parents]]
                 (cond
                   node
                   (step (conj path (.key node))
                         (into (into stack (list nodes))
                               (list (trie/children node)))
                         (cons node (cons parent parents)))
                   (and parent (not= 0 (.key parent)))
                   (lazy-seq
                    (cons (clojure.lang.MapEntry.
                           (rest path)
                           (let [^java.nio.ByteBuffer byte-buffer (.byte-buffer parent)]
                             (wrap-byte-buffer
                              byte-buffer
                              (.limit byte-buffer ^Integer (.limit parent))
                              (.position byte-buffer ^Integer (.address parent))
                              (value-decode-fn byte-buffer))))
                          (step (pop path)
                                stack
                                parents)))
                   :else nil))]
      (step [] (list (list trie)) '()))))

(defn -value [^TightlyPackedTrie trie value-decode-fn]
  (let [^java.nio.ByteBuffer byte-buffer (.byte-buffer trie)]
    (wrap-byte-buffer
     byte-buffer
     (.limit byte-buffer ^Integer (.limit trie))
     (.position byte-buffer ^Integer (.address trie))
     (value-decode-fn byte-buffer))))

(defn children- [^java.nio.ByteBuffer
                 byte-buffer
                 ^Integer address
                 ^Integer limit
                 value-decode-fn]
  (wrap-byte-buffer
   byte-buffer
   (.limit byte-buffer limit)
   (.position byte-buffer address)
   (let [val (value-decode-fn byte-buffer)
         size-of-index (encoding/decode byte-buffer)]
     (.limit byte-buffer ^Integer (+ (.position byte-buffer)
                                     size-of-index))
     (loop [children []]
       (if (= (.position byte-buffer) (.limit byte-buffer))
         children
         (let [child-key (encoding/decode-number-from-tightly-packed-trie-index byte-buffer)
               child-offset (encoding/decode-number-from-tightly-packed-trie-index byte-buffer)]
           (recur
            (conj
             children
             (TightlyPackedTrie.
              byte-buffer
              child-key
              (- address child-offset)
              (.capacity byte-buffer)
              value-decode-fn)))))))))

(def children-memo (memoize children-))

(defmethod print-method TightlyPackedTrie [trie ^java.io.Writer w]
  (print-method (into {} trie) w))

(defmethod print-dup TightlyPackedTrie [trie ^java.io.Writer w]
  (print-ctor trie (fn [o w] (print-dup (into {} trie) w)) w))

(defn tightly-packed-trie
  [^TightlyPackedTrie trie
   value-encode-fn
   value-decode-fn]
  (let [^ByteArrayOutputStream baos (ByteArrayOutputStream.)]
    (loop [nodes (seq trie)
           current-offset 8
           previous-depth 0
           child-indexes []]
      (let [^trie/Trie current-node (first nodes)
            current-depth (count (first current-node))]
        (cond
          (empty? nodes)
          (let [^clojure.lang.PersistentVector child-index (last child-indexes)
                child-index-baos (ByteArrayOutputStream.)
                _ (->> child-index
                       (run!
                        (fn [[key offset]]
                          (.write
                           child-index-baos
                           (encoding/encode-key-to-tightly-packed-trie-index key))
                          (.write
                           child-index-baos
                           (encoding/encode-offset-to-tightly-packed-trie-index
                            (- current-offset offset))))))
                child-index-byte-array (.toByteArray child-index-baos)
                size-of-child-index (encoding/encode (count child-index-byte-array))
                root-address current-offset
                value #^bytes (value-encode-fn 0)]
            (.write baos value)
            (.write baos size-of-child-index)
            (.write baos child-index-byte-array)
            (let [#^bytes ba (.toByteArray baos)
                  ^java.nio.ByteBuffer byte-buf (java.nio.ByteBuffer/allocate (+ 8 (count ba)))]
              (do (.putLong byte-buf root-address)
                  (.put byte-buf ba)
                  (.rewind byte-buf)
                  (->TightlyPackedTrie
                   byte-buf
                   0
                   (.getLong byte-buf)
                   (.capacity byte-buf)
                   value-decode-fn))))

          ;; Gone up from depth to a parent.
          ;; Process index of children.
          (> previous-depth current-depth)
          (let [[k v] (first nodes)
                value #^bytes (value-encode-fn v)
                child-index (last child-indexes)
                child-index-baos (ByteArrayOutputStream.)
                _ (->> child-index
                       (run!
                        (fn [[key offset]]
                          (.write
                           child-index-baos
                           (encoding/encode-key-to-tightly-packed-trie-index key))
                          (.write
                           child-index-baos
                           (encoding/encode-offset-to-tightly-packed-trie-index
                            (- current-offset offset))))))
                child-index-byte-array (.toByteArray child-index-baos)
                size-of-child-index (encoding/encode (count child-index-byte-array))
                current-index (last (pop child-indexes))]
            (.write baos value)
            (.write baos size-of-child-index)
            (.write baos child-index-byte-array)
            (recur (rest nodes)
                   (+ current-offset
                      (count value)
                      (count size-of-child-index)
                      (count child-index-byte-array))
                   current-depth
                   (conj (pop (pop child-indexes))
                         (conj current-index
                               [(last k)
                                current-offset]))))
          ;; Down or even in depth to children
          ;; Start keeping track of new children index
          :else
          (let [[k v] (first nodes)
                value #^bytes (value-encode-fn v)
                size-of-child-index (encoding/encode 0)
                child-indexes (into child-indexes
                                    (vec (repeat (- current-depth previous-depth) [])))
                current-child-index (last child-indexes)]
            (.write baos value)
            (.write baos size-of-child-index)
            (recur (rest nodes)
                   (+ current-offset
                      (count value)
                      (count size-of-child-index))
                   current-depth
                   (conj (pop child-indexes)
                         (conj current-child-index
                               [(last k)
                                current-offset])))))))))

(defn trie->children-at-depth
  [[[node & nodes] & stack] [parent & parents] min-depth max-depth]
  (let [current-depth (count (cons parent parents))]
    (cond
      (and node (< current-depth (dec max-depth)))
      (trie->children-at-depth
       (into (into stack (list nodes))
             (list (trie/children node)))
       (cons node (if parent (cons parent parents) nil))
       min-depth
       max-depth)
      (and parent (>= current-depth min-depth))
      (lazy-seq
       (cons (clojure.lang.MapEntry.
              (rest (reverse (map #(.key %) (cons parent parents))))
              (get parent []))
             (trie->children-at-depth stack (sequence parents) min-depth max-depth)))
      parent
      (trie->children-at-depth stack (sequence parents) min-depth max-depth)
      :else
      nil)))

;; TODO: Shared "save" interface for Trie?
(defn save-tightly-packed-trie-to-file
  [filepath trie]
  (with-open [o (java.io.FileOutputStream. filepath)]
    (.rewind (.byte-buffer trie))
    (.write (.getChannel o)
            (.byte-buffer trie))))

(defn load-tightly-packed-trie-from-file
  [filepath value-decode-fn]
  (with-open [i (io/input-stream filepath)
              baos (ByteArrayOutputStream.)]
    (io/copy i baos)
    (let [^java.nio.ByteBuffer byte-buffer (java.nio.ByteBuffer/wrap (.toByteArray baos))]
      (.rewind byte-buffer)
      (->TightlyPackedTrie
       byte-buffer
       0
       (.getLong byte-buffer)
       (.capacity byte-buffer)
       value-decode-fn))))
