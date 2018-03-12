(ns parseltongue.core-test
  (:require [clojure.test :refer :all]
            [parseltongue.core :refer :all]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

;; for reference from data/csv
(def ^{:private true} simple
  "1,2,3
4,5,6
7,8,9")

;; Most of these methods should be private and not tested here
;; My lack of comfort with Clojure, and my pleasure in testing
;; created ample incentive to write tests for most functions

(deftest enparening
  (let [x (enparen "test")]
    (is (clojure.core/= x "(test)"))))

(deftest enquoting
  (let [x (enquote 'r)]
    (is (clojure.core/= x '(quote r)))))

(deftest reading
  (let [x (string->parseltongue "col sum < 1")]
    (is (clojure.core/= x '(col '(sum < 1))))))

(deftest unreading
 (let [x (parseltongue->string '(col '(sum < 1)))]
  (is (clojure.core/= x "col sum < 1"))))

(deftest colling
  (let [x (col '(length < 1) 0 0 "1,2,3\n4,5,6\n7,8,9")
        y (col '(> 1) 0 0 "1,2,3\n4,5,6\n7,8,9")]
    (is x)
    (is y)))

(deftest rowing
  (let [x (row '(length > 1) 0 0 "1,2,3\n4,5,6\n7,8,9")
        y (row '(> 1) 0 0 "1,2,3\n4,5,6\n7,8,9")]
    (is x)
    (is y)))

(deftest celing
  (let [x (cel '(< 10) 0 0 "1,2,3\n4,5,6\n7,8,9")
        y (cel '(> 1) 0 0 "1,2,3\n4,5,6\n7,8,9")]
    (is x)
    (is y)))

(deftest celling
  (let [x (cell '(< 10) 0 0 "1,2,3\n4,5,6\n7,8,9")
        y (cell '(> 1) 0 0 "1,2,3\n4,5,6\n7,8,9")]
    (is x)
    (is y)))

(deftest summing
  (is (= (sum '("1" "2" "3")) 6)))

(deftest lengthing
  (is (= (length '("1" "2" "3")) 3)))

(deftest <ing
  (is (parseltongue.core/< 1 '["0" "0"]))
  (is (parseltongue.core/< 1 0)))

(deftest >ing
  (is (parseltongue.core/> 1 '["2" "2"]))
  (is (parseltongue.core/> 1 2)))

(deftest <=ing
  (is (parseltongue.core/<= 1 '["0" "0" "1"]))
  (is (parseltongue.core/<= 1 0))
  (is (parseltongue.core/<= 1 1)))

(deftest >=ing
  (is (parseltongue.core/>= 1 '["2" "2" "1"]))
  (is (parseltongue.core/>= 1 2))
  (is (parseltongue.core/>= 1 1)))

(deftest =ing
  (is (parseltongue.core/= 1 '["1" "1" "1"]))
  (is (parseltongue.core/= 1 1)))