(ns com.owoga.tightly-packed-trie.core
  (:require [clojure.zip :as zip]
            [com.owoga.tightly-packed-trie.encoding :as encoding]
            [com.owoga.tightly-packed-trie.bit-manip :as bm]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import (java.io ByteArrayOutputStream ByteArrayInputStream
                    DataOutputStream DataInputStream)))

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


;; It's convenient to work with hash-map representations
;; while developing.
;;
;; {"T" {:children:
;;       "I": {:value "IT"}
;;       "A": {:value "AT"}}}
;;
;; If you want to work with a hash-map representation and
;; want this library to handle conversion of the trie,
;; then your hash-map version will need to follow a few conventions.
;;
;; For example, this code expects child nodes to be key/value pairs under
;; the :children key.


;; To pack a trie into a contiguous array of bytes
;; and still be able to find a key in a list of
;; child indexes in an efficient way, the child
;; indexes need to be sorted so they can be
;; binary-searched.
;;
;; When working with a hash-map-backed Trie,
;; it's convenient to use update-in to add
;; new values to the trie. But the default
;; update-in creates unsorted hash-maps when it encounters
;; a new key. This has the same functionality of update-in,
;; but new keys are given sorted-map values.
(defn update-in-sorted
  "'Updates' a value in a nested associative structure, where ks is a
  sequence of keys and f is a function that will take the old value
  and any supplied args and return the new value, and returns a new
  nested structure.  If any levels do not exist, hash-maps will be
  created."
  {:added "1.0"
   :static true}
  ([m ks f & args]
     (let [up (fn up [m ks f args]
                (let [m (or m (sorted-map))
                      [k & ks] ks]
                  (assert (instance? clojure.lang.PersistentTreeMap m)
                          (apply str
                                 "A non-sorted hash-map in a sorted"
                                 "hash-map will probably be the seed of some problems."))
                  (if ks
                    (assoc m k (up (get m k) ks f args))
                    (assoc m k (apply f (get m k) args)))))]
       (up m ks f args))))


(defn children-of-map-trie-node
  "Grab the children of a node.

  A node is a map of {node-key {:children {,,,} ,,,}}.

  This functions gives you the child nodes not as a single map
  where all the child keys are part of the same map, but instead as a
  a seq where each value is a single node.

  This is a useful helper for turning a Trie from a map into a
  depth-first post-order traversable zipper."
  [node]
  (let [[k {:keys [children]}] (first (seq node))]
    (->> children
         (map (partial apply hash-map)))))

(comment
  (let [root-node {:root {:children {"T" {:some 'val} "U" {:other 'val}}}}]
    (children-of-map-trie-node root-node))
  ;; => ({"T" {:some val}} {"U" {:other val}})
  )

(defn without-children [node]
  (let [[k v] (first (seq node))]
    {k (dissoc v :children)}))

(defn map->depth-first-post-order-traversable-zipperable-vector
  [node]
  [(vec
    (map
     map->depth-first-post-order-traversable-zipperable-vector
     (children-of-map-trie-node node)))
   (without-children node)])

(comment
  ;; This comment demonstrates how we change the order in which
  ;; we traverse the map. If we don't turn each node into a vector
  ;; where the list of children are first, then the parent node would
  ;; get traversed befor the children. In the example below, the
  ;; "AT" node would be traversed befor the "SAT" and "TAT" nodes.
  (let [m {:root
           {:children
            {"T"
             {:children
              {"A" {:children
                    {"T" {:value "TAT", :count 1}
                     "S" {:value "SAT" :count 1}}
                    :value "AT"
                    :count 1},
               "U" {:children {"T" {:value "TUT", :count 1}}}}}}}}]
    (let [z (zip/vector-zip
             (map->depth-first-post-order-traversable-zipperable-vector m))]
      (->> z
           (iterate zip/next)
           (take-while (complement zip/end?))
           (map zip/node)
           (filter map?))))
  ;; => ({"T" {:value "TAT", :count 1}}
  ;;     {"S" {:value "SAT", :count 1}}
  ;;     {"A" {:value "AT", :count 1}}
  ;;     {"T" {:value "TUT", :count 1}}
  ;;     {"U" {}}
  ;;     {"T" {}}
  ;;     {:root {}})
  )

(defn depth-first-post-order-traversable-zipperable-vector->map
  "Parity reversal of the code above. Maps are easier to reason about
  while developing. It's just inconvenient to traverse them in the
  order needed by the algorithms we use to pack them into a contiguous
  array of byte for the tightly packed trie."
  [node]
  (let [children (first node)
        parent (second node)
        [parent-key parent-val] (first (seq parent))]
    (sorted-map
     parent-key
     (assoc
      parent-val
      :children
      (into
       (sorted-map)
       (map depth-first-post-order-traversable-zipperable-vector->map children))))))

(comment
  (let [m {:root
           {:children
            {"T"
             {:children
              {"A" {:children
                    {"T" {:value "TAT", :count 1}
                     "S" {:value "SAT" :count 1}}
                    :value "AT"
                    :count 1},
               "U" {:children {"T" {:value "TUT", :count 1}}}}}}}}]
    (let [vect (map->depth-first-post-order-traversable-zipperable-vector m)]
      (depth-first-post-order-traversable-zipperable-vector->map vect)))
  ;; => {:root
  ;;     {:children
  ;;      {"T"
  ;;       {:children
  ;;        {"A"
  ;;         {:value "AT",
  ;;          :count 1,
  ;;          :children
  ;;          {"S" {:value "SAT", :count 1, :children {}},
  ;;           "T" {:value "TAT", :count 1, :children {}}}},
  ;;         "U" {:children {"T" {:value "TUT", :count 1, :children {}}}}}}}}}
  )

;;;; Zipper utilities
;;
;; Some useful functions for traversing/transforming zippers.
;;
;; This is what a tree looks like.
;;
;;      [1 [2 [3 4 5] [6 7]]]
;;      {                   }
;;            /    \
;;           /      \
;;          1     [2 [3 4 5] [6 7]]
;;                   /    |     \
;;                  /     |      \
;;                 2    [3 4 5]  [6 7]
;;
;; Clojure's zipper gives us convenient ways
;; to iterate over the nodes of the zipper.
;;
;; We can use the functions below to
;; recreate the functionality of
;; (map #(if (even? %) (* % %) %) coll)
;; but with zippers instead of collections.
(defn visitor-next
  "Visits every loc in a zipper and calls zip/next with the result of applying f
  to the loc.

  Goes without saying that f should return a loc."
  [zipper f]
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (f loc))))))

(defn visitor-prev
  "visitor-next in reverse."
  [zipper f]
  (loop [loc zipper]
    (if (nil? (zip/prev loc))
      (zip/root (f loc))
      (recur (zip/prev (f loc))))))

(defn visitor-filter
  "Helper for traversing a zipper with a visitor function.

  Convenient for pulling filtering logic out of a visitor function
  into composable and specific filter functions."
  [pred visitor]
  (fn [loc]
    (if (pred loc)
      (visitor loc)
      loc)))

(comment
  ;; This comment shows an example of using the visitor
  ;; helpers to recreate the functionality similar to
  ;; (->> [1 2 3 4 5]
  ;;      (map #(if (even? %) (* % %) %))
  ;;
  ;; This first example has the conditional logic inside
  ;; the visitor function.
  (let [zipper (zip/vector-zip [1 [2 [3 4 5] [6 7]]])]
    (visitor-next
     zipper
     #(if (and (int? (zip/node %)) (even? (zip/node %)))
        (zip/edit % (fn [n] (int (Math/pow n 2))))
        %)))
  ;; => [1 [4 [3 16 5] [36 7]]]
  ;;
  ;; This second example has a seperate filter predicate
  ;; from the transformation visitor and combines them
  ;; with the visitor-filter function.
  (let [zipper    (zip/vector-zip [1 [2 [3 4 5] [6 7]]])
        pred      (fn [loc]
                    (let [node (zip/node loc)]
                      (and (int? node)
                           (even? node))))
        transform (fn [loc]
                    (zip/edit loc (fn [n] (int (Math/pow n 2)))))]
    (visitor-next zipper (visitor-filter pred transform)))
  ;; => [1 [4 [3 16 5] [36 7]]]
  )

(defn loc-children
  "Takes a zipper loc and returns seq of children locs.

  Written to work with zippers of a particular structure:
  [[child1, child2, ,,,] parent]"
  [loc]
  (if (and (zip/left loc)
           (zip/down (zip/left loc)))
    (let [children (zip/down (zip/left loc))]
      (->> children
           (iterate zip/right)
           (take-while (complement nil?))))))

(comment
  (let [v [[1 2 3] :parent]
        z (zip/vector-zip v)]
    (->> z
         zip/down
         zip/right
         loc-children
         (map zip/node)))
  ;; => (1 2 3)
  )

;;;; Tightly Packing Tries
;;
;; These next functions are all helpers
;; related to byte-packing nodes in preperation
;; for writing them to a byte stream.

(defn previous-node [loc]
  (loop [loc (zip/prev loc)]
    (cond
      (nil? loc) nil
      (map? (zip/node loc)) loc
      :else (recur (zip/prev loc)))))

(defn loc->byte-address
  "Given a loc without a byte-address, calculate it from the previous loc.

  0x00|node val of previous loc  |
  0x01|size of child index       |
  0x02|child1 key                |
  0x03|child1 byte address offset|
  0x04|child2 key                |
  0x05|child2 byte address offset|
  0x??|node val of current loc   |

  We obviously wouldn't need this if we were writing directly to a
  ByteBuffer. Whatever position we are at is our address.

  But if we want to maintain the byte-packed data as
  part of the map- or vector-like trie structure, then
  we need this."
  ([loc]
   (loc->byte-address loc 0))
  ([loc starting-offset]
   (let [prev (previous-node loc)]
       (if prev
         (let [[k {:keys [byte-address byte-array]}] (first (seq (zip/node prev)))]
           (+ byte-address (count byte-array)))
         starting-offset))))

(defn child->index
  "Given a child gets a map with info needed to build an index.

  The below info is just a little bit of lagniappe. The only thing we're
  pulling off the child are the values of the keys :byte-address and :key.
  The encoding will happen later.
  But for reference, the encoding will be:
  The index will be a list of pairs of variable-length encoded bytes.
  The first number of the pair, the bytes will be encoded with a flag bit of 1.
  The second number of the pair, the bytes will be encoded with a flag bit of 0."
  [child]
  (let [[k {:keys [byte-address byte-array] :as v}] (first (seq child))]
    {:byte-address byte-address
     :key k}))

(defn child-node-key-and-offset-from-parent-as-byte-array
  [{:keys [key offset]}]
  (let [baos (ByteArrayOutputStream.)]
    (.write baos (encoding/encode-key-to-tightly-packed-trie-index key))
    (.write baos (encoding/encode-offset-to-tightly-packed-trie-index offset))
    (.toByteArray baos)))

(defn pack-node-value
  "Returns byte-array of node value.
  Byte-array is 2 variable-length encoded numbers.
  For a markov trie, this would be an number ID
  of the n-gram and an number of the frequency.

  Nodes without terminal values get a value and count of 0."
  [node]
  (let [baos (ByteArrayOutputStream.)]
    (.write baos (encoding/encode (get node :value 0)))
    (.write baos (encoding/encode (get node :count 0)))
    (.toByteArray baos)))

(defn transform-trie-add-byte-pack-to-each-node
  "Visitor for a vector-based trie of structure [[child1, child2, ,,,,] parent].

  Transforms each node adding keys for a byte-array of the node and its children index key/offsets
  as well as a key for its own offset.

  Starts at byte 8, reserving the first 8 bytes for the for root address won't be known
  until the end of the zipper.

  Transforming the trie to add these keys is an intermediary step that can probably be
  bypassed in the future by writing directly to a ByteArrayOutputStream."
  [loc]
  (let [baos (ByteArrayOutputStream.)
        ;; Byte-address of the current node. Gets calculated from the
        ;; previous node's byte address and the size of the previous
        ;; node's byte array.
        byte-address (loc->byte-address loc 8)
        child-nodes (->> loc
                         loc-children
                         (map (comp second zip/node)))
        ;; For child, we need to know the offset of the child node's address
        ;; from this parent node's address.
        children (map
                  (fn [child-node]
                    (let [child-index (child->index child-node)]
                      (assoc
                       child-index
                       :offset
                       (- byte-address (:byte-address child-index)))))
                  child-nodes)
        ;; Create the byte array of the index of the children
        index-ba (let [index-baos (ByteArrayOutputStream.)
                       child-byte-arrays
                       (map
                        child-node-key-and-offset-from-parent-as-byte-array
                        children)]
                   (loop [bas child-byte-arrays]
                     (if (empty? bas)
                       (.toByteArray index-baos)
                       (do (.write index-baos (first bas))
                           (recur (rest bas))))))]
    (zip/edit
     loc
     (fn [node]
       (let [[k v] (first (seq node))]
         (.write baos (pack-node-value v))
         (.write baos (encoding/encode (count index-ba)))
         (.write baos index-ba)
         {k (conj v {:byte-address byte-address
                     :byte-array (.toByteArray baos)})})))))

(defprotocol ITrie
  (as-map [this] "Map that underlies trie.")
  (as-vec [this] "Depth-first post-order vector.")
  (as-byte-array [this] (str "Add key/values to each node containing"
                             " the tightly-packed byte-array representation of the node."))
  (transform [this f] "Depth-first post-order apply each function to each node."))

(def not-found# (gensym))

(deftype Trie [f trie]
  ITrie
  (as-map [_] trie)
  (as-vec [_] (map->depth-first-post-order-traversable-zipperable-vector trie))
  (as-byte-array [self]
    (->> (transform
          self
          (visitor-filter
           #(map? (zip/node %))
           transform-trie-add-byte-pack-to-each-node))))
  (transform [self f]
    (->> self
         as-vec
         zip/vector-zip
         (#(visitor-next % f))
         depth-first-post-order-traversable-zipperable-vector->map
         (Trie. f)))

  ;; By returning lookups in the same strucure as root-level nodes
  ;; but with the root at the found descendent, we can treat this
  ;; descendant as a new root Trie node. This gives us the advantage
  ;; of being able to re-use the Trie functions on sub-nodes.
  clojure.lang.ILookup
  (valAt [_ ks]
    (let [v (get-in trie (cons :root (interleave (repeat :children) ks)) not-found#)]
      (if (= v not-found#)
        (throw (Exception. (format "Key not found: %s" ks)))
        (Trie. f (sorted-map (last ks) v)))))
  (valAt [_ ks not-found]
    (Trie. f (sorted-map
              (last ks)
              (get-in trie (cons :root (interleave (repeat :children) ks)) not-found))))

  clojure.lang.IPersistentCollection
  (seq [self]
    (->> self
         as-vec
         zip/vector-zip
         (iterate zip/next)
         (take-while (complement zip/end?))
         (map zip/node)
         (filter map?)
         (filter (comp :value second first seq))))
  (cons [_ o]
    (let [path (cons :root (interleave (repeat :children) (butlast o)))
          node (get-in trie path)]
      (Trie. f (update-in-sorted trie path f o))))
  (empty [_] (Trie. f {}))
  (equiv [_ o]
    (and (isa? (class o) Trie)
         (= (as-map o) trie))))

(defn trie
  ([] (->Trie
       (fn update-fn [prev cur]
         (if (nil? prev)
           {:value (last cur)
            :count 1}
           (-> prev
               (update :count (fnil inc 0))
               (assoc :value (last cur)))))
       (sorted-map)))
  ([& ks]
   (reduce
    (fn [t k]
      (conj t k))
    (trie)
    ks)))

(comment
  (let [v1 '(1 2 3 123)
        v2 '(1 2 12)
        v3 '(1 2 2 122)
        v4 '(1 3 1 131)
        t1 (trie v1 v2 v3 v4)]
    {:as-vec (as-vec (get t1 '(1 2)))
     :as-map (as-map (get t1 '(1 2)))
     :as-byte-array (as-byte-array (get t1 '(1 2)))})
  ;; => {:as-vec
  ;;     [[[[] {2 {:value 122, :count 1}}] [[] {3 {:value 123, :count 1}}]]
  ;;      {2 {:count 1, :value 12}}],
  ;;     :as-map
  ;;     {2
  ;;      {:children {2 {:value 122, :count 1}, 3 {:value 123, :count 1}},
  ;;       :count 1,
  ;;       :value 12}},
  ;;     :as-byte-array
  ;;     {2
  ;;      {:byte-address 14,
  ;;       :byte-array [-116, -127, -124, -126, 6, -125, 3],
  ;;       :children
  ;;       {2
  ;;        {:value 122,
  ;;         :count 1,
  ;;         :byte-address 8,
  ;;         :byte-array [-6, -127, -128],
  ;;         :children {}},
  ;;        3
  ;;        {:value 123,
  ;;         :count 1,
  ;;         :byte-address 11,
  ;;         :byte-array [-5, -127, -128],
  ;;         :children {}}},
  ;;       :count 1,
  ;;       :value 12}}}
  )

(defn rewind-to-key [bb stop]
  (loop []
    (let [current (.get bb (.position bb))
          previous (.get bb (dec (.position bb)))]
      (if (or (= stop (.position bb))
              (and (encoding/key-byte? current)
                   (encoding/offset-byte? previous)))
        bb
        (do (.position bb (dec (.position bb)))
            (recur))))))

(defn forward-to-key [bb stop]
  (loop []
    (if (or (= stop (.position bb))
            (and (encoding/key-byte? (.get bb (.position bb)))
                 (encoding/offset-byte?
                  (.get bb (inc (.position bb))))))
      bb
      (do (.position bb (inc (.position bb)))
          (recur)))))

(defn find-key-in-index
  [bb target-key max-address not-found]
  (.limit bb max-address)
  (let [key
        (loop [previous-key nil
               min-position (.position bb)
               max-position max-address]
          (if (zero? (- max-position min-position))
            not-found
            (let [mid-position (+ min-position (quot (- max-position min-position) 2))]
              (.position bb mid-position)
              (let [bb (rewind-to-key bb min-position)
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
    {:value value
     :count freq}))

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

(defprotocol ITightlyPackedTrie
  (children [self] "Immediate children of a node.")
  (value [self] "Return node value, disassociated with children."))

(deftype TightlyPackedTrie [byte-buffer key address limit]
  ITightlyPackedTrie
  (value [self]
    (wrap-byte-buffer
     byte-buffer
     (.limit byte-buffer limit)
     (.position byte-buffer address)
     (tightly-packed-trie-node-value byte-buffer)))
  (children [self]
    (wrap-byte-buffer
     byte-buffer
     (.limit byte-buffer limit)
     (.position byte-buffer address)
     (let [val (encoding/decode byte-buffer)
           freq (encoding/decode byte-buffer)
           size-of-index (encoding/decode byte-buffer)]
       (.limit byte-buffer (+ (.position byte-buffer)
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
                (.capacity byte-buffer))))))))))

  clojure.lang.ILookup
  (valAt [self ks]
    (wrap-byte-buffer
     byte-buffer
     (.limit byte-buffer limit)
     (.position byte-buffer address)
     (if (empty? ks)
       self
       (let [val (encoding/decode byte-buffer)
             freq (encoding/decode byte-buffer)
             size-of-index (encoding/decode byte-buffer)
             offset (find-key-in-index
                     byte-buffer
                     (first ks)
                     (+ (.position byte-buffer) size-of-index)
                     :not-found)]
         (if (= offset :not-found)
           (throw (Exception. (format "Index not found %s" ks)))
           (let [child (TightlyPackedTrie.
                        byte-buffer
                        (first ks)
                        (- address offset)
                        (.capacity byte-buffer))]
             (get child (rest ks))))))))
  (valAt [self ks not-found]
    (wrap-byte-buffer
     byte-buffer
     (.limit byte-buffer limit)
     (.position byte-buffer address)
     (if (empty? ks)
       self
       (let [val (encoding/decode byte-buffer)
             freq (encoding/decode byte-buffer)
             size-of-index (encoding/decode byte-buffer)
             offset (find-key-in-index
                     byte-buffer
                     (first ks)
                     (+ (.position byte-buffer) size-of-index)
                     :not-found)]
         (if (= offset :not-found)
           not-found
           (let [child (TightlyPackedTrie.
                        byte-buffer
                        (first ks)
                        (- address offset)
                        (.capacity byte-buffer))]
             (get child (rest ks)))))))))

(defn tightly-packed-trie
  "Assumes the trie has been transformed so that each node
  includes a :byte-array key to the byte array that needs to be written
  for that node and a :byte-address key that has been calculated
  with an offset of 8. (The first 8 bytes are reserved for the root address.)"
  [trie]
  (let [baos (ByteArrayOutputStream.)
        trie (as-byte-array trie)]
    ;; This transform writes to the ByteArrayOutputStream.
    (transform
     trie
     (visitor-filter
      #(map? (zip/node %))
      (fn [loc]
        (let [{:keys [byte-array]} (second (first (seq (zip/node loc))))]
          (.write baos byte-array)
          loc))))
    (let [ba (.toByteArray baos)
          root-address (get-in (as-map trie) [:root :byte-address])
          byte-buf (java.nio.ByteBuffer/allocate (+ 8 (count ba)))]
      (.putLong byte-buf root-address)
      (.put byte-buf ba)
      (.rewind byte-buf)
      (->TightlyPackedTrie byte-buf 0 (.getLong byte-buf) (.capacity byte-buf)))))

(defn zipper-tpt
  "Turns a tightly-packed trie into a zipper.
  Since the byte buffer that backs the trie can't be edited,
  `make-node` and the zipper edit functions won't work."
  [tpt]
  (let [branch? (fn branch? [node]
                  (and (instance? TightlyPackedTrie node)
                       (not-empty (children node))))
        zipper-children (fn zippper-children [node]
                          (children node))
        make-node (fn make-node [node children]
                    (throw (Exception. "Can't add children to Tightly Packed Trie nodes.")))]
    (zip/zipper branch? zipper-children make-node tpt)))

(comment
  (let [v1 '(1 2 3 123)
        v2 '(1 2 12)
        v3 '(1 2 2 122)
        v4 '(1 3 1 131)
        t1 (trie v1 v2 v3 v4)
        tpt (tightly-packed-trie t1)]
    (->> tpt
         zipper-tpt
         (iterate zip/next)
         (take-while (complement zip/end?))
         (map zip/node)
         (map #(hash-map (.key %) (value %)))))

  )

;; TODO: Shared "save" interface for Trie?
(defn save-tightly-packed-trie-to-file
  [filepath trie]
  (with-open [o (io/output-stream filepath)]
    (.write o (.array (.byte-buffer trie)))))

(defn load-tightly-packed-trie-from-file
  [filepath]
  (with-open [i (io/input-stream filepath)
              baos (ByteArrayOutputStream.)]
    (io/copy i baos)
    (let [byte-buffer (java.nio.ByteBuffer/wrap (.toByteArray baos))]
      (.rewind byte-buffer)
      (->TightlyPackedTrie byte-buffer 0 (.getLong byte-buffer) (.capacity byte-buffer)))))
