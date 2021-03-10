(ns com.owoga.trie
  (:require [clojure.zip :as zip]))

(defn trie->depth-first-post-order-traversable-zipperable-vector
  ([path node]
   (vec
    (map
     (fn [[k v]]
       [(trie->depth-first-post-order-traversable-zipperable-vector (conj path k) v)
        (clojure.lang.MapEntry. (conj path k) (.value v))])
     (.children- node)))))

(defn depth-first-post-order-traversable-zipperable-vector->trie
  [cls [children [key node]]]
  (sorted-map
   (last key)
   (cls (.key node) (.value node)
        (into (sorted-map)
              (map depth-first-post-order-traversable-zipperable-vector->trie children)))))

(declare ->Trie)

(defn -without
  [trie [k & ks]]
  (if k
    (if-let [next-trie (get (.children- trie) k)]
      (let [next-trie-without (-without next-trie ks)
            new-trie (->Trie (.key trie)
                             (.value trie)
                             (if next-trie-without
                               (assoc (.children- trie) k next-trie-without)
                               (dissoc (.children- trie) k)))]
        (if (and (empty? new-trie)
                 (nil? (.value new-trie)))
          nil
          new-trie)))
    (if (seq (.children- trie))
      (->Trie
       (.key trie)
       nil
       (.children- trie))
      nil)))

(defprotocol ITrie
  (children [self] "Immediate children of a node.")
  (lookup [self ks] "Return node at key."))

(deftype Trie [key value children-]
  ITrie
  (children [trie]
    (map
     (fn [[k child]]
       (Trie. k
              (.value child)
              #_(sorted-map)
              (.children- child)))
     children-))

  (lookup [trie k]
    (loop [k' k
           trie' trie]
      (cond
        ;; Allows `update` to work the same as with maps... can use `fnil`.
        ;; (nil? trie') (throw (Exception. (format "Key not found: %s" k)))
        (nil? trie') nil
        (empty? k')
        (Trie. (.key trie')
               (.value trie')
               (.children- trie'))
        :else (recur
               (rest k')
               (get (.children- trie') (first k'))))))

  clojure.lang.ILookup
  (valAt [trie k]
    (loop [k' k
           trie' trie]
      (cond
        ;; Allows `update` to work the same as with maps... can use `fnil`.
        ;; (nil? trie') (throw (Exception. (format "Key not found: %s" k)))
        (nil? trie') nil
        (empty? k') (.value trie')
        :else (recur
               (rest k')
               (get (.children- trie') (first k'))))))

  (valAt [trie k not-found]
    (loop [k' k
           trie' trie]
      (cond
        (nil? trie') not-found
        (empty? k') (.value trie')
        :else (recur
               (rest k')
               (get (.children- trie') (first k'))))))

  clojure.lang.IPersistentCollection
  (cons [trie entry]
    (cond
      (instance? Trie (second entry))
      (assoc trie (first entry) (.value (second entry)))
      :else
      (assoc trie (first entry) (second entry))))

  (empty [trie]
    (Trie. key nil (sorted-map)))

  (equiv [trie o]
    (and (= (.value trie)
            (.value o))
         (= (.children- trie)
            (.children- o))
         (= (.key trie)
            (.key o))))

  clojure.lang.Associative
  (assoc [trie opath ovalue]
    (if (empty? opath)
      (Trie. key ovalue children-)
      (Trie. key value (update
                        children-
                        (first opath)
                        (fnil assoc (Trie. (first opath) nil (sorted-map)))
                        (rest opath)
                        ovalue))))
  (entryAt [trie key]
    (clojure.lang.MapEntry. key (get trie key)))
  (containsKey [trie key]
    (boolean (get trie key)))

  clojure.lang.IPersistentMap
  (assocEx [trie key val]
    (if (contains? trie key)
      (throw (Exception. (format "Value already exists at key %s." key)))
      (assoc trie key val)))
  (without [trie key]
    (-without trie key))

  clojure.lang.Counted
  (count [trie]
    (count (seq trie)))

  clojure.lang.Seqable
  (seq [trie]
    (->> trie
         ((partial trie->depth-first-post-order-traversable-zipperable-vector []))
         zip/vector-zip
         (iterate zip/next)
         (take-while (complement zip/end?))
         (map zip/node)
         (filter (partial instance? clojure.lang.MapEntry))
         (#(if (empty? %) nil %)))))

(defn make-trie
  ([]
   (->Trie '() nil (sorted-map)))
  ([& ks]
   (reduce
    (fn [t kv]
      (conj t kv))
    (make-trie)
    (partition 2 ks))))

(comment
  (make-trie "do" "do" "dot" "dot" "dog" "dog")
  ;; => {[\d \o \g] "dog", [\d \o \t] "dot", [\d \o] "do"}

  ;; Access values at a particular key with get
  ;; and the key (as any seqable).
  (let [trie (make-trie "do" "do" "dot" "dot" "dog" "dog")]
    (get trie [\d \o]))
  ;; => "do"

  ;; Access children with `traverse`
  (let [trie (make-trie "do" "do" "dot" "dot" "dog" "dog" "doggo" "fluffy")]
    (traverse trie "do"))
  ;; => {[\g \g \o] "fluffy", [\g] "dog", [\t] "dot"}

  ;; Any seq of comparable elements will work for keys.
  ;; But they *must* be comparable, so you can't mix-and-match
  ;; different types.
  (make-trie '(:k :e :y) 1 '(:k :e :e) 2)
  ;; => {[:k :e :e] 2, [:k :e :y] 1}

  ;; Seqing over a trie will return the elements in
  ;; depth-first post-order traversal with children sorted by key.
  (->> (make-trie '(1 2 4) 124 '(1 2) 12 '(1 2 3) 123 '(1 2 2) 122)
       (map (fn [[k v]]
              [k (* 2 v)]))
       (into (make-trie)))
  ;; => {[1 2 2] 244, [1 2 3] 246, [1 2 4] 248, [1 2] 24}
  )
