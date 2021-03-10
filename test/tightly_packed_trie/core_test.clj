(ns tightly-packed-trie.core-test
  "Basic tests for the primary API of `next.jdbc`."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [com.owoga.tightly-packed-trie.core :as tpt]))

(deftest basic-tests
  ;; use ds-opts instead of (ds) anywhere you want default options applied:
  (testing "map-based trie"
    (let [trie (tpt/trie)]
      (testing "key not found"
        (is (thrown-with-msg?
             Exception
             #"Key not found"
             (get trie '("foo"))))
        (is (= :not-found
               (get trie '("foo") :not-found))))
      (testing "conjing to trie"
        (let [trie (conj trie '("d" "o" "g" "dog"))]
          (is (instance? com.owoga.tightly_packed_trie.core.Trie (get trie '("d" "o" "g"))))
          (is (= (tpt/as-map (get trie '("d" "o" "g")))
                 {"g" {:value "dog" :count 1}})))))))
