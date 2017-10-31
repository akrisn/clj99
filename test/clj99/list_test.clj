(ns clj99.list-test
  "List Tests"
  (:require [clojure.test :refer :all]
            [clj99.list :refer :all]))

(deftest p01-test
  (testing "Find the last box of a list"
    (is (= (my-last '(a b c d))
           '(d)))
    (is (= (my-last '(a))
           '(a)))
    (is (= (my-last '())
           nil))))


(deftest p02-test
  (testing "Find the last but one box of a list."
    (is (= (my-but-last '(a b c d))
           '(c d)))
    (is (= (my-but-last '(a))
           '(a)))
    (is (= (my-but-last '())
           nil))))

;; * (element-at '(a b c d e) 3)
;; C

(deftest p03-test
  (testing "Find the K'th element of a list."
    (is (= (element-at '(a b c d) 3)
           'c))))


(deftest p04-test
  (testing "Find the number of elements of a list."
    (is (= (my-count '()) 0))
    (is (= (my-count '(a)) 1))
    (is (= (my-count '(a b c d))
           4))))


(deftest p05-test
  (testing "Reverse a list."
    (is (= (my-reverse '()) '()))
    (is (= (my-reverse '(a)) '(a)))
    (is (= (my-reverse '(a b c d)) '(d c b a)))))


(deftest p06-test
  (testing "Find out whether a list is a palindrome."
    (is (= (palindrome? '()) true))
    (is (= (palindrome? '(a)) true))
    (is (= (palindrome? '(a a)) true))
    (is (= (palindrome? '(a b)) false))
    (is (= (palindrome? '(a b a)) true))
    (is (= (palindrome? '(a b b a)) true))
    (is (= (palindrome? '(a b b c)) false))
    (is (= (palindrome? '(a b a b)) false))
    (is (= (palindrome? '(x a m a x)) true))))


(deftest p07-test
  (testing "Flatten a nested list structure."
    (is (= (my-flatten '()) '()))
    (is (= (my-flatten '((a))) '(a)))
    (is (= (my-flatten '(a (b (c d) e)))
           '(a b c d e)))))


(deftest p08-test
  (testing "Eliminate consecutive duplicates of list elements."
    (is (= (compress '())
           '()))
    (is (= (compress '(a a a a b c c a a d e e e e))
           '(a b c a d e)))))


(deftest p09-test
  (testing "Pack consecutive duplicates of list elements into sublists."
    (is (= (pack '())
           '()))
    (is (= (pack '(a a a a b c c a a d e e e e))
           '((a a a a) (b) (c c) (a a) (d) (e e e e))))))


(deftest p10-test
  (testing "Run-length encoding of a list."
    (is (= (encode '())
           '()))
    (is (= (encode '(a a a a b c c a a d e e e e))
           '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))))))


(deftest p11-test
  (testing "Modified run-length encoding."
    (is (= (encode-modified '())
           '()))
    (is (= (encode-modified '(a a a a b c c a a d e e e e))
           '((4 a) b (2 c) (2 a) d (4 e))))))


(deftest p12-test
  (testing "Decode a run-length encoded list."
    (is (= (decode '()) '()))
    (is (= (decode '(a)) '(a)))
    (is (= (decode '(a b)) '(a b)))
    (is (= (decode '((3 a))) '(a a a)))
    (is (= (decode '((3 a) b)) '(a a a b)))
    (is (= (decode '((4 a) b (2 c) (2 a) d (4 e)))
                   '(a a a a b c c a a d e e e e)))))


(deftest p13-test
  (testing "Run-length encoding of a list (direct solution)."
    (is (= (encode-direct '())
           '()))
    (is (= (encode-direct '(a a a a b c c a a d e e e e))
           '((4 a) b (2 c) (2 a) d (4 e))))))


(deftest p14-test
  (testing "Duplicate the elements of a list."
    (is (= (dupli '()) '()))
    (is (= (dupli '(a b c c d))
           '(a a b b c c c c d d)))))


(deftest p15-test
  (testing "Replicate the elements of a list a given number of times."
    (is (= (repli '() 3) '()))
    (is (= (repli '(a b c c d) 3)
           '(a a a b b b c c c c c c d d d)))))


(deftest p16-test
  (testing "Drop every N'th element from a list."
    (is (= (drop '(a b c d e f g h i k) 3)
           '(a b d e g h k)))))

