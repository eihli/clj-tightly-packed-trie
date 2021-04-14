(ns com.owoga.trie)

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
              (.children- child)))
     children-))

  (lookup [trie k]
    (loop [k k
           trie trie]
      (cond
        ;; Allows `update` to work the same as with maps... can use `fnil`.
        ;; (nil? trie') (throw (Exception. (format "Key not found: %s" k)))
        (nil? trie) nil
        (empty? k)
        (Trie. (.key trie)
               (.value trie)
               (.children- trie))
        :else (recur
               (rest k)
               (get (.children- trie) (first k))))))

  clojure.lang.ILookup
  (valAt [trie k]
    (if-let [node (lookup trie k)]
      (.value node)
      nil))

  (valAt [trie k not-found]
    (or (get trie k) not-found))

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

  java.lang.Iterable
  (iterator [trie]
    (.iterator (seq trie)))

  clojure.lang.Counted
  (count [trie]
    (count (seq trie)))

  clojure.lang.Seqable
  (seq [trie]
    (let [step (fn step [path [[node & nodes] & stack] [parent & parents]]
                 (cond
                   node
                   (step (conj path (.key node))
                         (into (into stack (list nodes))
                               (list (children node)))
                         (cons node (cons parent parents)))
                   (and parent (not= '() (.key parent)))
                   (lazy-seq
                    (cons (clojure.lang.MapEntry.
                           (rest path)
                           (.value parent))
                          (step (pop path)
                                stack
                                parents)))
                   :else nil))]
      (step [] (list (list trie)) '()))))

(defmethod print-method Trie [trie ^java.io.Writer w]
  (print-method (into {} trie) w))

(defmethod print-dup Trie [trie ^java.io.Writer w]
  (print-ctor trie (fn [o w] (print-dup (into {} trie) w)) w))

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
