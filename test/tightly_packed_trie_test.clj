(ns tightly-packed-trie-test
  (:require  [clojure.test :refer [deftest is testing] :as t]
             [com.owoga.trie :as trie]
             [com.owoga.tightly-packed-trie :as tpt]
             [com.owoga.tightly-packed-trie.encoding :as encode]
             [com.owoga.tightly-packed-trie.encoding :as encoding]
             [com.owoga.tightly-packed-trie.bit-manip :as bm]))

(defn value-encode-fn [v]
  (if (or (= v ::tpt/root)
          (nil? v))
    (encode/encode 0)
    (encode/encode v)))

(defn value-decode-fn [byte-buffer]
  (let [v (encode/decode byte-buffer)]
    (if (zero? v)
      nil
      v)))

(deftest tightly-packed-trie-tests
  (let [empty-trie (-> (trie/make-trie)
                       (#(tpt/tightly-packed-trie % value-encode-fn value-decode-fn)))
        initialized-trie (->> '([1 3] 13 [1] 1 [1 2] 12)
                              (apply trie/make-trie)
                              (#(tpt/tightly-packed-trie % value-encode-fn value-decode-fn)))]
    (testing "ILookup"
      (is (= 13 (get initialized-trie [1 3])))
      (is (= :not-found (get initialized-trie [4] :not-found)))
      (is (= nil (get initialized-trie [4]))))
    (testing "ITrie"
      (testing "lookup"
        (is (= nil (trie/lookup empty-trie [1])))
        (is (= 1 (get (trie/lookup initialized-trie [1]) [])))
        (is (= 12 (get (trie/lookup initialized-trie [1]) [2]))))
      (testing "children"
        (is (= '(12 13)
               (map #(get % [])
                    (trie/children (trie/lookup initialized-trie [1])))))))))

(deftest extended-tightly-packed-trie-tests
  (let [initialized-trie (->> (trie/make-trie '(1 2 3) 123 '(1 2 1) 121 '(1 2 2) 122 '(1 3 1) 131)
                              (#(tpt/tightly-packed-trie % value-encode-fn value-decode-fn)))]
    (testing "ILookup"
      (is (= 123 (get initialized-trie [1 2 3])))
      (is (= :not-found (get initialized-trie [4] :not-found)))
      (is (= nil (get initialized-trie [4]))))
    (testing "ITrie"
      (testing "lookup"
        (is (= nil (trie/lookup initialized-trie [4])))
        (is (= nil (get (trie/lookup initialized-trie [1]) [])))
        (is (= nil (get (trie/lookup initialized-trie [1]) [2]))))
      (testing "children"
        (is (= '(121 122 123)
               (map #(get % [])
                    (trie/children (trie/lookup initialized-trie [1 2])))))))
    (testing "Seq"
      (is (= '([[1 2 1] 121]
               [[1 2 2] 122]
               [[1 2 3] 123]
               [[1 2]   nil]
               [[1 3 1] 131]
               [[1 3]   nil]
               [[1]     nil]
               [[]      nil])
             (seq initialized-trie))))))

(comment
  (let [trie (trie/make-trie '(1 2 3) 123 '(1 2 1) 121 '(1 2 2) 122 '(1 3 1) 131)
        tpt (tpt/tightly-packed-trie trie value-encode-fn value-decode-fn)
        byte-buffer (.byte-buffer tpt)]
    (tpt/wrap-byte-buffer
     byte-buffer
     (.limit byte-buffer (.limit tpt))
     (.position byte-buffer (.address tpt))
     [(value-decode-fn byte-buffer)
      (value-decode-fn byte-buffer)
      (encode/decode-number-from-tightly-packed-trie-index byte-buffer)
      (encode/decode-number-from-tightly-packed-trie-index byte-buffer)]))

  )
