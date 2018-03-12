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

(deftest enparening
  (let [x (enparen "test")]
    (is (clojure.core/= x "(test)"))))

(deftest enquoting
  (let [x (enquote 'r)]
    (is (clojure.core/= x '(quote r)))))

;; FIXME seems reader is broken somehow
(deftest reading
  (let [x (string->parseltongue "col sum < 1")]
    (is (clojure.core/= x '(col '(sum < 1))))))

(deftest unreading
 (let [x (parseltongue->string '(col '(sum < 1)))]
  (is (clojure.core/= x "col sum < 1"))))

(deftest <ing
  (is (parseltongue.core/< 1 '["0" "0"]))
  (is (parseltongue.core/< 1 '"0")))

(deftest >ing
  (is (parseltongue.core/> 1 '["2" "2"]))
  (is (parseltongue.core/> 1 '"2")))

(deftest <=ing
  (is (parseltongue.core/<= 1 '["0" "0" "1"]))
  (is (parseltongue.core/<= 1 '"0"))
  (is (parseltongue.core/<= 1 '"1")))

(deftest >=ing
  (is (parseltongue.core/>= 1 '["2" "2" "1"]))
  (is (parseltongue.core/>= 1 '"2"))
  (is (parseltongue.core/>= 1 '"1")))

(deftest =ing
  (is (parseltongue.core/= 1 '"1")))