;; Fast weighted random selection thanks to the Vose algorithm.
;; https://gist.github.com/ghadishayban/a26cc402958ef3c7ce61
(ns com.owoga.trie.math
  (:import clojure.lang.PersistentQueue))

;; Vose's alias method
;; http://www.keithschwarz.com/darts-dice-coins/
(defprotocol Rand
  (nextr [_ rng]))

(deftype Vose [n ^ints alias ^doubles prob]
  Rand
  ;; returns the index of the chosen weight
  (nextr [_ rng] ;; not using the rng for now
    (let [i (rand-int n)
          p (aget prob i)]
      (if (or (= p 1.0)
              (< (rand) p))
        i
        (aget alias i)))))

(defn ^:private make-vose [dist]
  (let [N (count dist)
        alias (int-array N)
        prob  (double-array N)]
    (if (zero? N)
      (->Vose N alias prob)
      (let [^doubles ps (->> dist
                             (map (partial * N))
                             (into-array Double/TYPE))

            [small large] (loop [i 0
                                 [small large] [PersistentQueue/EMPTY
                                                PersistentQueue/EMPTY]
                                 ps (seq ps)]
                            (if (seq ps)
                              (let [p (first ps)]
                                (if (< p 1)
                                  (recur (inc i)
                                         [(conj small i) large]
                                         (rest ps))
                                  (recur (inc i)
                                         [small          (conj large i)]
                                         (rest ps))))
                              [small large]))

            [small large] (loop [small small
                                 large large]
                            (if (and (seq small) (seq large))
                              (let [l (first small)
                                    g (first large)
                                    small (pop small)
                                    large (pop large)]
                                (aset-double prob l (aget ps l))
                                (aset-int alias l g)
                                (let [pg (- (+ (aget ps g) (aget ps l))
                                            1.0)]
                                  (aset-double ps g pg)
                                  (if (< pg 1)
                                    (recur (conj small g) large)
                                    (recur small (conj large g)))))
                              [small large]))]
        (doseq [g (concat large small)]
          (aset-double prob g 1))
        (->Vose N alias prob)))))

(defn from-weights [ws]
  (let [tot (reduce + 0.0 ws)]
    (assert (> tot 0) "Can't Vose RNG from 0 weights.")
    (let [dist (map #(/ % tot) ws)]
      (make-vose (vec dist)))))

(comment
  (let [ws [1 2 1 3 3]
        rng (from-weights ws)
        chosen (repeatedly 1000000 #(nextr rng nil))
        accuracy (mapv (comp float
                             #(/ % 100000)
                             (frequencies chosen))
                       (range (count ws)))]
    accuracy))

(defn weighted-selection
  "If given a coll, assumes the coll is weights and returns the selected index by
  weighted random selection.

  If given a key function and a collection, uses the key function to get a
  collection of weights and returns the value at the randomly selected index."
  ([coll]
   (assert (not-empty coll) "Can't select from empty coll")
   (let [rng (from-weights coll)
         index (nextr rng nil)]
     index))
  ([key-fn coll]
   (assert (not-empty coll) "Can't select from empty coll")
   (let [rng (from-weights (map key-fn coll))
         index (nextr rng nil)
         selection (nth coll index)]
     selection)))
