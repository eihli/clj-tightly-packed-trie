(ns markov-language-model
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [com.owoga.tightly-packed-trie.core :as tpt]
            [com.owoga.tightly-packed-trie.encoding :as encoding]
            [cljol.dig9 :as d]
            [clojure.zip :as zip]
            [com.owoga.tightly-packed-trie.bit-manip :as bm]))

(def corpus (slurp (io/resource "cask_of_amontillado.txt")))

;; For better generation of text, you'll probably want to pad the starts
;; of sentences with n-1 "start-of-sentence" tokens.
(defn prep-punctuation-for-tokenization
  "Puts spaces around punctuation so that they aren't
  tokenized with the words they are attached to.

  Might add extraneous whitespace, but presumedly that will be ignored/removed
  during tokenization."
  [text]
  (string/replace text #"([\.,!?])" " $1 "))

(defn remove-formatting-characters
  "Input has underscores, presumably because the text
  might be rendered by something that can italicize or bold text.
  We'll just ignore them for now."
  [text]
  (string/replace text #"[_*]" ""))

(defn tokenize [text]
  (-> text
      remove-formatting-characters
      prep-punctuation-for-tokenization
      (string/split #"[\n ]+")))

(defn interleave-all
  "Like interleave, but instead of ending the interleave when the shortest collection
  has been consumed, continues to interleave the remaining collections."
  {:added "1.0"
   :static true}
  ([] ())
  ([c1] (lazy-seq c1))
  ([c1 c2]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2)]
      (if (and s1 s2)
        (cons (first s1) (cons (first s2)
                               (interleave-all (rest s1) (rest s2))))
        (lazy-seq (or s1 s2))))))
  ([c1 c2 & colls]
   (lazy-seq
    (let [ss (->> (map seq (conj colls c2 c1))
                  (remove nil?))]
      (when ss
        (concat (map first ss) (apply interleave-all (map rest ss))))))))

(comment
  (let [tokens [1 2 3 4 5]
        p1 (partition 1 1 tokens)
        p2 (partition 2 1 tokens)
        p3 (partition 3 1 tokens)]
    (interleave-all p1 p2 p3)))

(defn ngramify-tokens [n m tokens]
  (let [partition-colls (map #(partition % 1 tokens) (range n m))
        ngrams (apply interleave-all partition-colls)]
    ngrams))

(comment
  (->> (tokenize corpus)
       (take 5)
       (ngramify-tokens 1 4)) ;; => (("The")
  ;;     ("The" "thousand")
  ;;     ("The" "thousand" "injuries")
  ;;     ("thousand")
  ;;     ("thousand" "injuries")
  ;;     ("thousand" "injuries" "of")
  ;;     ("injuries")
  ;;     ("injuries" "of")
  ;;     ("injuries" "of" "Fortunato")
  ;;     ("of")
  ;;     ("of" "Fortunato")
  ;;     ("Fortunato"))
  )

(defn make-trie
  ([] (tpt/->Trie
       (fn update-fn [prev cur]
         (if (nil? prev)
           (sorted-map
            :value (last cur)
            :count 1)
           (-> prev
               (update :count (fnil inc 0))
               (assoc :value (last cur)))))
       (sorted-map)))
  ([& ks]
   (reduce
    (fn [t k]
      (conj t k))
    (make-trie)
    ks)))

(defn add-terminal-value-to-ngram
  "The Trie expects entries to be of the form '(k1 k2 k3 value).
  The ngrams generated above are just '(k1 k2 k3).
  This adds a value that is simply the ngram itself:
  '(k1 k2 k3 '(k1 k2 k3))."
  [ngram]
  (concat ngram (list ngram)))

(comment
  (let [ngrams (->> corpus
                    tokenize
                    (take 200)
                    (ngramify-tokens 1 4)
                    (map add-terminal-value-to-ngram))
        trie (apply make-trie ngrams)]
    (tpt/as-map trie))
  ;; {:root
  ;;  {:children
  ;;   {","
  ;;    {:children
  ;;     {"I"
  ;;      {:children {"vowed" {:count 1, :value ("," "I" "vowed")}},
  ;;       :count 1,
  ;;       :value ("," "I")},
  ;;      "and"
  ;;      {:children {"he" {:count 1, :value ("," "and" "he")}},
  ;;       :count 1,
  ;;       :value ("," "and")},
  ;;       ,,,}}}}}}

  )

;; TODO: Move this to ITrie?
(defn trie->seq-of-nodes
  "Returns a seq of every terminal node. Useful for things like
  doing aggregation calculations."
  [trie]
  (->> trie
       tpt/as-vec
       zip/vector-zip
       (iterate zip/next)
       (take-while (complement zip/end?))
       (map zip/node)
       (filter map?)))

;; The tightly packed trie uses an encoding where integers are encoded with
;; variable lengths. To maximize memory efficiency, the most commonly used values
;; should have the smallest integer IDs. That way the values that most commonly appear
;; are encoded with the fewest bytes.
(defn seq-of-nodes->sorted-by-count
  "Sorted first by the rank of the ngram, lowest ranks first.
  Sorted second by the frequency of the ngram, highest frequencies first.
  This is the order that you'd populate a mapping of keys to IDs."
  [nodes]
  (->> nodes
       (map (comp first seq))
       (map (fn [[k v]]
              (vector (:value v) (:count v))))
       ;; root node and padded starts
       (remove (comp nil? second))
       (sort-by #(vector (count (first %))
                         (- (second %))))))

(comment
  (let [ngrams (->> corpus
                    tokenize
                    (take 200)
                    (ngramify-tokens 1 4)
                    (map add-terminal-value-to-ngram))
        trie (apply make-trie ngrams)]
    (->> trie
         trie->seq-of-nodes
         seq-of-nodes->sorted-by-count
         (take 10)))
  ;; => ([(",") 11]
  ;;     [(".") 9]
  ;;     [("I") 8]
  ;;     [("the") 6]
  ;;     [("to") 6]
  ;;     [("was") 5]
  ;;     [("a") 4]
  ;;     [("my") 4]
  ;;     [("of") 4]
  ;;     [("as") 3])
  )

(defn trie->database
  "It's convenient to work with a trie that has keys and values as
  human-readable strings, as pulled straight from a corpus in the case
  of a markov trie. But to tightly pack the trie into a byte array,
  we need every value to be an integer that we can variable-length-encode.

  This creates a database for conveniently converting the human-readable
  entries to ids and back from ids to human-readable entries."
  [trie]
  (let [sorted-keys (->> (trie->seq-of-nodes trie)
                         seq-of-nodes->sorted-by-count)]
    (loop [sorted-keys sorted-keys
           database {}
           i 1]
      (if (empty? sorted-keys)
        database
        (recur
         (rest sorted-keys)
         (-> database
             (assoc (first (first sorted-keys))
                    {:count (second (first sorted-keys))
                     :id i})
             (assoc i (first (first sorted-keys))))
         (inc i))))))

(comment
  (let [ngrams (->> corpus
                    tokenize
                    (take 200)
                    (ngramify-tokens 1 4)
                    (map add-terminal-value-to-ngram))
        trie (apply make-trie ngrams)]
    (trie->database trie))
 ;; {("at") {:count 1, :id 39},
 ;; 453 ("revenge" "." "You"),
 ;; ("The") {:count 1, :id 37},
 ;; ("resolved" ",") {:count 1, :id 256},
 ;; 487 ("very" "definitiveness" "with"),
 ;; ("be" "respected") {:count 1, :id 170},
 ;; ("a" "point") {:count 1, :id 158},
 ;; 357 ("and" "he" "did"),
 ;; 275 ("the" "very"),
 ;; ("doubt" "my" "good") {:count 1, :id 381},
 ;; ,,,}
  )

(seq {"and" {:count 1 :value '("foo")}});; => (["and" {:count 1, :value ("foo")}])

(defn transform-trie->ids
  "Once we have a database to convert from string-keys to integer-ids and back,
  we can traverse the trie using its `transform` zipper and `zip/edit` each
  node replacing the string-keys with their integer-ids."
  [trie database]
  (let [transform-p #(map? (zip/node %))
        transform-f
        (fn tf [loc]
          (zip/edit
           loc
           (fn [node]
             ;; {"And {:count 1, :value (! " "And)}} ;; <- Node
             (let [[k v] (first (seq node))]
               {(get-in database [(list k) :id] (if (= k :root) :root))
                (assoc v :value (get-in database [(:value v) :id] 0))}))))]
    (tpt/transform trie (tpt/visitor-filter transform-p transform-f))))

(def trie
  (let [ngrams (->> corpus
                    tokenize
                    (ngramify-tokens 1 4)
                    (map add-terminal-value-to-ngram))]
    (apply make-trie ngrams)))

(def trie-database
  (trie->database trie))

(def tpt-ready-trie (transform-trie->ids trie trie-database))

(def tightly-packed-trie
  (tpt/tightly-packed-trie tpt-ready-trie))

;;;; DEMO
;;
(comment
;;;; Our "database" (just a hash-map) serves a dual purpose.
  ;;
  ;; It maps n-grams to their frequency counts and to an integer ID.
  ;; It also maps that integer ID back to the n-gram.
  (take 10 trie-database)
  ;; => ([("Then" "I") {:count 1, :id 1475}]
  ;;     [("to" "your" "long") {:count 1, :id 5371}]
  ;;     [("an" "instant" "he") {:count 1, :id 3842}]
  ;;     [("fifth" "," "the") {:count 1, :id 4209}]
  ;;     [("from" "the" "depth") {:count 1, :id 4270}]
  ;;     [2721 ("the" "more")]
  ;;     [("during" "which" ",") {:count 1, :id 4144}]
  ;;     [("nodded") {:count 1, :id 674}]
  ;;     [("the" "feeble") {:count 1, :id 2693}]
  ;;     [("intermingling" "," "into") {:count 1, :id 4488}])


;;;; We can `get` the value of an n-gram from a Trie.
  ;; The value returned will be a Trie that has as its root node the
  ;; value at the n-gram. This gives you access to all of the descendants.
  ;;
  ;; Having access to the descendants is useful for something like
  ;; auto-complete. You can get in the trie the input to the completion, the prefix.
  ;; Then you can get the completions by simple seq-ing over the child nodes.
  (tpt/as-map (get trie '("," "I")))

  ;; => {"I"
  ;;     {:count 10,
  ;;      :value ("," "I"),
  ;;      :children
  ;;      {"am" {:count 1, :value ("," "I" "am")},
  ;;       "began" {:count 2, :value ("," "I" "began")},
  ;;       ,,,
  ;;       "well" {:count 1, :value ("," "I" "well")}}}}

;;;; Database
  ;; Each n-gram has its own integer ID. The integer IDs should be handed
  ;; out to n-grams in order of frequency. Therefore, you're 1-grams will probably
  ;; have lower IDs than the higher-order n-grams.
  ;;
  ;; Here we see "," is the 2nd most-common n-gram.
  (get-in trie-database ['(",") :id]) ;; => 2
  (get-in trie-database ['("I") :id]) ;; => 4
  ;; The ID of a 2-gram is not related in any way to
  ;; the two 1-grams that make it up. Every n-gram is unique
  ;; and gets its own unique ID.
  ;;
  ;; BUT... Every node is referenced by a 1-gram key.
  ;; So the 2-gram '("," "I") is referenced from
  ;; the :root key's children by the 1-gram key '(",")
  ;; and then by that 1-gram key's children by the 1-gram '("I").
  ;; The VALUE of that node though is the 2-gram '("," "I").
  ;;
  ;; To re-iterate: The keys are all 1-grams at every nesting level.
  ;; The values are the higher-order n-grams the lower in the nesting
  ;; that you go.
  (get-in trie-database ['("," "I") :id]) ;; => 911

;;;; Map-based Trie vs Tightly Packed Trie
;;;;
  ;; The interface is *almost* the same between the two.
  ;; Tightly packed tries can't be updated or written to.
  ;; They can only be read.
  ;; And to get from integer IDs to human-readable strings,
  ;; you need to go through the database.
  ;;
  ;; Other than that though, let's see it in action!
  ;;
;;;; Here is the map-based trie.
  (->> (tpt/as-map (get trie '("," "I")))
       (#(get-in % ["I" :children]))
       (map seq)
       (map first))
  ;; => ("am" "began" "ceased" "had" "resumed" "soon" "suffered" "vowed" "well")

;;;; And here is the tightly-packed trie
  (->> (get tightly-packed-trie '(2 4))
       tpt/children
       (map tpt/value)
       (map :value)
       (map #(get trie-database %))
       (map last)
       sort)
  ;; => ("am" "began" "ceased" "had" "resumed" "soon" "suffered" "vowed" "well")

;;;; Ta-da!
;;;; Let's check the size difference

  ;; Memory footprint comparison
  ;; 2.2mb -> 37kb.
  ;; 1.7% of its original Clojure map size!!!
  (->> trie (.trie) vector d/sum)

  ;; 65485 objects
  ;; 109687 references between them
  ;; 2179088 bytes total in all objects
  ;; no cycles
  ;; 8413 leaf objects (no references to other objects)
  ;; 1 root nodes (no reference to them from other objects _in this graph_)

  (->> tightly-packed-trie (.byte-buffer) vector d/sum)
  ;; 2 objects
  ;; 1 references between them
  ;; 37680 bytes total in all objects
  ;; no cycles
  ;; 1 leaf objects (no references to other objects)
  ;; 1 root nodes (no reference to them from other objects _in this graph_)


  (let [trie-at-2 (get tightly-packed-trie '(2))
        address (.address trie-at-2)
        byte-buffer (.byte-buffer trie-at-2)
        limit (.limit trie-at-2)]
    (tpt/wrap-byte-buffer
     byte-buffer
     (.position byte-buffer address)
     (.limit byte-buffer limit)
     (println "Address of node at 2" address)
     (encoding/decode byte-buffer)
     (encoding/decode byte-buffer)
     (println "Size of index at 2" (encoding/decode byte-buffer))
     (println "position of first key in index" (.position byte-buffer))
     (encoding/decode-number-from-tightly-packed-trie-index byte-buffer)
     (encoding/decode-number-from-tightly-packed-trie-index byte-buffer)
     (.position byte-buffer (- address 1037)) ;; Position of '("," "the")
     (encoding/decode byte-buffer)
     (encoding/decode byte-buffer)
     (encoding/decode byte-buffer) ;; 11 size of index
     ;; 1462 position of buffer
     ;; max-address of index = 1473 (or 1472?)
     (.position byte-buffer)
     (.position byte-buffer 2618) ;; First mid of broken binary search
     (tpt/rewind-to-key byte-buffer 2500)
     (println (.position byte-buffer))
     (println (bm/to-binary-string (.get byte-buffer (.position byte-buffer))))
     (println (bm/to-binary-string (.get byte-buffer (dec (.position byte-buffer)))))
     
     ))








  (let [trie-at-2 (get tightly-packed-trie '(2))
        address (.address trie-at-2)
        byte-buffer (.byte-buffer trie-at-2)
        limit (.limit trie-at-2)]
    (tpt/wrap-byte-buffer
     byte-buffer
     (.position byte-buffer address)
     (.limit byte-buffer limit)
     (encoding/decode byte-buffer)
     (encoding/decode byte-buffer)
     (encoding/decode byte-buffer)
     (encoding/decode-number-from-tightly-packed-trie-index byte-buffer)
     (encoding/decode-number-from-tightly-packed-trie-index byte-buffer)
     (.position byte-buffer (- address 1037))
     (encoding/decode byte-buffer)
     (encoding/decode byte-buffer)
     (encoding/decode byte-buffer)))

  ;; I's offset, 986.
  ;; See below. ID at offset 986 is 2! Same as at above offset!
  ;; And the count is 4? The count coincidentally is the ID we expect?
  (let [trie-at-2 (get tightly-packed-trie '(2))
        address 986
        byte-buffer (.byte-buffer trie-at-2)
        limit (.limit trie-at-2)]
    (tpt/wrap-byte-buffer
     byte-buffer
     (.position byte-buffer address)
     (.limit byte-buffer limit)
     (encoding/decode byte-buffer)))

  (get trie-database 4)
  (let [byte-buffer (.byte-buffer tightly-packed-trie)]
    (.position byte-buffer)))

;; Memory footprint comparison
;; 2.2mb -> 32kb.
;; 1.5% of its original Clojure map size!
(comment
  (->> trie (.trie) vector d/sum)
  ;; 65485 objects
  ;; 109687 references between them
  ;; 2179088 bytes total in all objects
  ;; no cycles
  ;; 8413 leaf objects (no references to other objects)
  ;; 1 root nodes (no reference to them from other objects _in this graph_)

  (->> tightly-packed-trie (.byte-buffer) vector d/sum)
  ;; 2 objects
  ;; 1 references between them
  ;; 32896 bytes total in all objects
  ;; no cycles
  ;; 1 leaf objects (no references to other objects)
  ;; 1 root nodes (no reference to them from other objects _in this graph_)
  )


(defn key-get-in-tpt [tpt db ks]
  (let [id (map #(get-in db [(list %) :id]) ks)
        v (get tpt id)]
    {id v}))

(defn id-get-in-tpt [tpt db ids]
  (let [ks (apply concat (map #(get db %) ids))
        v (get tpt ids)
        id (get-in db [ks :id])]
    {ks (assoc v :value (get db id))}))

(comment
  (key-get-in-tpt
   tightly-packed-trie
   trie-database
   '("another"))

  ;; => {(2 2 3) {:value 3263, :count 462}}
  (id-get-in-tpt
   tightly-packed-trie
   trie-database
   '(2 2 3))
  ;; => {("<s>" "<s>" "the") {:value ("<s>" "<s>" "the"), :count 462}}
  )

(comment
  ;; database
  (let [texts (->> (dark-corpus-file-seq 500 2)
                   (map slurp))
        trie (create-trie-from-texts texts)]
    (->> (trie->database trie)
         (#(get % 3))))

  (let [texts (->> (dark-corpus-file-seq 500 2)
                   (map slurp))
        trie (create-trie-from-texts texts)]
    (tpt/as-map (transform-trie->ids trie)))

  (let [texts (->> (dark-corpus-file-seq 500 2)
                   (map slurp))
        trie (create-trie-from-texts texts)
        tightly-packed-trie (tpt/tightly-packed-trie
                             (transform-trie->ids trie))]
    (get tightly-packed-trie '(2 2 3)))


  (let [texts (->> (dark-corpus-file-seq 500 2)
                   (map slurp))
        trie (create-trie-from-texts texts)]
    (tpt/as-map trie))

  (let [text (slurp (first (dark-corpus-file-seq 500 1)))]
    (->> text
         util/clean-text
         (#(string/split % #"\n+"))))

  )
