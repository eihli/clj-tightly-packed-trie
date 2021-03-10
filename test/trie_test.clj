(ns trie-test
  (:require [clojure.test :refer [deftest is testing use-fixtures] :as t]
            [com.owoga.trie :as trie]
            [clojure.zip :as zip]
            [clojure.main :as main]))

(deftest trie-tests
  (let [empty-trie (trie/make-trie)
        initialized-trie (trie/make-trie '(1 2) 12)]
    (testing "assoc"
      (is (= (assoc empty-trie '(1 2) 12)
             initialized-trie)))
    (testing "dissoc"
      (let [expected (-> (trie/make-trie)
                         (assoc '(1) 1))
            trie (-> (trie/make-trie)
                     (assoc '(1) 1)
                     (assoc '(1 3) 13))]
        (is (= expected (dissoc trie '(1 3))))))
    (testing "ILookup"
      (is (= 12 (get initialized-trie '(1 2))))
      (is (= :not-found (get initialized-trie '(1 3) :not-found)))
      (is (nil? (get initialized-trie '(1 3)))))
    (testing "IPersistentCollection"
      (is (empty? empty-trie))
      (is (= (conj empty-trie ['(1 2) 12])
             initialized-trie)))
    (testing "Counted"
      (is (zero? (count empty-trie)))
      (is (= 2 (count initialized-trie))))
    (testing "Seqable"
      (is (= '([[1 2] 12] [[1] nil])
             (seq initialized-trie))))))

