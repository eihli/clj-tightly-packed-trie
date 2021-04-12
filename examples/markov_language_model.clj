(ns markov-language-model
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [com.owoga.trie.math :as math]
            [com.owoga.tightly-packed-trie :as tpt]
            [com.owoga.tightly-packed-trie.encoding :as encoding]
            [com.owoga.trie :as tr]
            [cljol.dig9 :as d]
            [clojure.zip :as zip]
            [com.owoga.tightly-packed-trie.bit-manip :as bm]))

(def corpus (slurp (io/resource "cask_of_amontillado.txt")))

(defn prep-punctuation-for-tokenization
  "Puts spaces around punctuation so that they aren't
  tokenized with the words they are attached to.

  Might add extraneous whitespace, but presumedly that will be ignored/removed
  during tokenization."
  [text]
  (string/replace text #"([\.,!?])" " $1 "))

;; For better generation of text, you'll probably want to pad the starts
;; of sentences with n-1 "start-of-sentence" tokens.
(defn add-bol-and-eol-tokens [text]
  (-> text
      (string/replace #"(\.)" "</s> . <s>")
      (#(str "<s> " %))))

(defn remove-quotes
  [text]
  (string/replace text #"\"" ""))

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
      remove-quotes
      add-bol-and-eol-tokens
      string/lower-case
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

    [p1
     p2
     p3
     (interleave-all p1 p2 p3)])
  ;; => [((1) (2) (3) (4) (5))
  ;;     ((1 2) (2 3) (3 4) (4 5))
  ;;     ((1 2 3) (2 3 4) (3 4 5))
  ;;     ((1) (1 2) (1 2 3) (2) (2 3) (2 3 4) (3) (3 4) (3 4 5) (4) (4 5) (5))]
  )

(defn ngramify-tokens [n m tokens]
  (let [partition-colls (map #(partition % 1 tokens) (range n m))
        ngrams (apply interleave-all partition-colls)]
    ngrams))

(comment
  (->> (tokenize corpus)
       (take 5)
       (ngramify-tokens 1 4))
  ;; => (("the")
  ;;     ("the" "thousand")
  ;;     ("the" "thousand" "injuries")
  ;;     ("thousand")
  ;;     ("thousand" "injuries")
  ;;     ("thousand" "injuries" "of")
  ;;     ("injuries")
  ;;     ("injuries" "of")
  ;;     ("injuries" "of" "fortunato")
  ;;     ("of")
  ;;     ("of" "fortunato")
  ;;     ("fortunato"))
  )

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
                  (map add-terminal-value-to-ngram))]))

(defn trie->database
  "It's convenient to work with a trie that has keys and values as
  human-readable strings, as pulled straight from a corpus in the case
  of a markov trie. But to tightly pack the trie into a byte array,
  we need every value to be an integer that we can variable-length-encode.

  This creates a database for conveniently converting the human-readable
  entries to ids and back from ids to human-readable entries.

  Ids will start at 1 so that 0 can be reserved for the root node."
  [trie]
  (let [sorted-keys (->> (seq trie)
                         (sort-by (fn [[k v]]
                                    (:count v)))
                         (reverse))]
    (loop [sorted-keys sorted-keys
           database {}
           i 1]
      (if (empty? sorted-keys)
        database
        (recur
         (rest sorted-keys)
         (-> database
             (assoc (first (first sorted-keys))
                    (assoc (second (first sorted-keys)) :id i))
             (assoc i (first (first sorted-keys))))
         (inc i))))))

(comment
  (take 10 (trie->database trie))
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

(defn transform-trie->ids
  "Once we have a database to convert from string-keys to integer-ids and back,
  we can traverse the trie replacing the string-keys with their integer-ids."
  [trie database]
  (->> trie
       (map
        (fn [[k v]]
          [(vec (map #(get (get database [%]) :id) k))
           {:id (get-in database [k :id])
            :count (get-in database [k :count])}]))
       (into (tr/make-trie))))

(def trie
  (let [ngrams (->> corpus
                    tokenize
                    (ngramify-tokens 1 4)
                    (map add-terminal-value-to-ngram)
                    (map (fn [entry]
                           (list (butlast entry)
                                 (last entry)))))]
    (->> ngrams
         (reduce
          (fn [acc [k v]]
            (update
             acc
             k
             (fnil
              (fn [existing]
                (update existing :count inc))
              {:value v
               :count 0})))
          (tr/make-trie)))))

(comment
  (take 10 (drop 1000 trie))
  ;; => ([["be" "awaiting"] {:value ("be" "awaiting"), :count 1}]
  ;;     [["be" "cautious" "as"] {:value ("be" "cautious" "as"), :count 1}]
  ;;     [["be" "gone"] {:value ("be" "gone"), :count 2}]
  ;;     [["be" "ill" ","] {:value ("be" "ill" ","), :count 1}])
  )

(def trie-database
  (trie->database trie))

(comment
  (take 4 trie-database)
  ;; => ([0 ["."]]
  ;;     [["to" "your" "long"] {:value ("to" "your" "long"), :count 1, :id 1119}]
  ;;     [["an" "instant" "he"] {:value ("an" "instant" "he"), :count 1, :id 4800}]
  ;;     [["fifth" "," "the"] {:value ("fifth" "," "the"), :count 1, :id 3919}])
  )

(def tpt-ready-trie (transform-trie->ids trie trie-database))

(comment
  (take 4 tpt-ready-trie)
  ;; => ([[0 1 27] {:id 5082, :count 1}]
  ;;     [[0 1 104] {:id 5072, :count 1}]
  ;;     [[0 1 112] {:id 5075, :count 1}]
  ;;     [[0 1 146] {:id 5077, :count 1}])

  )

(defn value-encode-fn [v]
  (if (and (number? v) (zero? v))
    (encoding/encode 0)
    (byte-array
     (concat (encoding/encode (:id v))
             (encoding/encode (:count v))))))

(defn value-decode-fn [byte-buffer]
  (let [id (encoding/decode byte-buffer)]
    (if (zero? id)
      {:id id}
      {:id id
       :count (encoding/decode byte-buffer)})))

(def tightly-packed-trie
  (tpt/tightly-packed-trie tpt-ready-trie value-encode-fn value-decode-fn))

;;;; DEMO
;;;; ** Out of date since new TrieAgain code
(comment
;;;; Size comparisons
  ;;
  ;; Original trie, keys and values are lists and strings.
  ;; ~1,900 kb
  (d/sum [trie])
  ;; 61112 objects
  ;; 103249 references between them
  ;; 1901656 bytes total in all objects
  ;; no cycles
  ;; 8421 leaf objects (no references to other objects)

  ;; Original trie, keys and values numbers
  ;; ~900 kb
  (d/sum [tpt-ready-trie])
  ;; 30008 objects
  ;; 62543 references between them
  ;; 907992 bytes total in all objects
  ;; no cycles
  ;; 5438 leaf objects (no references to other objects)

  ;; Tightly-packed-trie, keys and values numbers (backed by var-len encoded ints)
  ;; ~36 kb
  (d/sum [tightly-packed-trie])
  ;; 6 objects
  ;; 5 references between them
  ;; 36736 bytes total in all objects
  ;; no cycles
  ;; 4 leaf objects (no references to other objects)

;;;; Size comparison summary
  ;;
  ;; Condensed original: 900 kb
  ;; Tightly packed:     36  kb
  ;; Compression:        ~96% !!!


;;;; Getting value from each type of trie
  ;;
  (get trie ["<s>" "i" "was"])
  ;; => {:value ("<s>" "i" "was"), :count 1}

  (get tpt-ready-trie [0 8 21])
  ;; => {:id 5116, :count 1}

  (get tightly-packed-trie [0 8 21])
  ;; => {:id 5116, :count 1}

  ;; And then to get back to a string version, use the database.
  (->> [0 8 21]
       (get tightly-packed-trie)
       :id
       (get trie-database)
       (get trie-database))
  ;; => {:value ("<s>" "i" "was"), :count 1, :id 5116}

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

;;;; Database
  ;; Each n-gram has its own integer ID. The integer IDs should be handed
  ;; out to n-grams in order of frequency. Therefore, you're 1-grams will probably
  ;; have lower IDs than the higher-order n-grams.
  ;;
  ;; Here we see "," is the 2nd most-common n-gram.
  (get-in trie-database ['(",") :id]) ;; => 7
  (get-in trie-database ['("i") :id]) ;; => 8
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
  (get-in trie-database ['("," "i") :id]) ;; => 23

;;;; Trie vs Tightly Packed Trie
;;;;
  ;; The interface is *almost* the same between the two.
  ;; Tightly packed tries can't be updated or written to.
  ;; They can only be read.
  ;; And to get from integer IDs to human-readable strings,
  ;; you need to go through the database.
  ;;
  ;; Other than that though, let's see it in action!
  ;;
;;;; Here is the Trie
  (get trie '("i"))
  ;; => {:value ("i"), :count 107}

  (->> (tr/lookup trie '("i"))
       (take 5))
  ;; => ([["," "i"] {:value ("i" "," "i"), :count 1}]
  ;;     [[","] {:value ("i" ","), :count 1}]
  ;;     [["again" "paused"] {:value ("i" "again" "paused"), :count 1}]
  ;;     [["again"] {:value ("i" "again"), :count 1}]
  ;;     [["am" "on"] {:value ("i" "am" "on"), :count 1}])

  (->> (tr/lookup trie '("i"))
       (tr/children)
       (map #(get % []))
       (take 5))
  ;; => ({:value ("i" ","), :count 1}
  ;;     {:value ("i" "again"), :count 1}
  ;;     {:value ("i" "am"), :count 1}
  ;;     {:value ("i" "answered"), :count 1}
  ;;     {:value ("i" "began"), :count 2})

;;;; And here is the tightly-packed trie
  (->> (tr/lookup tightly-packed-trie '(8))
       (tr/children)
       (map #(get % []))
       (take 5))
  ;; => ({:id 3392, :count 1}
  ;;     {:id 3270, :count 1}
  ;;     {:id 129, :count 5}
  ;;     {:id 70, :count 9}
  ;;     {:id 69, :count 9})

  (->> (tr/lookup tightly-packed-trie '(8))
       (tr/children)
       (map #(get % []))
       (take 5)
       (map #(get trie-database (:id %))))
  ;; => (["i" ","] ["i" "to"] ["i" "was"] ["i" "had"] ["i" "said"])
;;;; Ta-da!
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
   '("i" "will"))
  ;; => {(8 49) {:id 3257, :count 1}}

  (id-get-in-tpt
   tightly-packed-trie
   trie-database
   '(8 49))
  ;; => {("i" "will") {:id 3257, :count 1, :value ["i" "will"]}}

  )

;;;; Markov-generating text from trie
(comment
  (def example-story
    (loop [generated-text [(:id (get trie-database ["<s>"]))]
           i 0]
      (if (> i 100)
        generated-text
        (recur
         (conj
          generated-text
          (tpt/.key
           (math/weighted-selection
            #(:count (get % []))
            (loop [i 3
                   children
                   (tr/children
                    (tr/lookup
                     tightly-packed-trie
                     (vec (take-last i generated-text))))]
              (if (empty? children)
                (recur (dec i)
                       (tr/children
                        (tr/lookup
                         tightly-packed-trie
                         (vec (take-last i generated-text)))))
                children)))))
         (inc i)))))

  (->> example-story
       (map #(get trie-database %))
       (apply concat)
       (remove #{"<s>" "</s>"})
       (string/join " ")
       (#(string/replace % #" ([\.,\?])" "$1"))
       ((fn [txt]
          (string/replace txt #"(^|\. |\? )([a-z])" (fn [[a b c]]
                                                      (str b (.toUpperCase c)))))))

  ;; => "I broke and reached him a flagon of de grave. We came at length. He again took my arm, and holding the flambeaux over the wall; i replied, were a great and numerous family. Whither? to your long life. Putting on a tight-fitting parti-striped dress, and descending again, and had given them explicit orders not to be found, and this time i made bold to seize fortunato by an arm above the elbow. In its destined position."
  )
