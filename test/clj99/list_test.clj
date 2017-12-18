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
           ()))))


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


(deftest p17-test
  (testing "Split a list into two parts; the length of the first part is given."
    (is (= (split '(a b c d e f g h i k) 3)
           '((a b c) (d e f g h i k))))))


(deftest p18-test
  (testing "Extract a slice from a list."
    (is (= (slice '(a b c d e f g h i k) 3 7)
           '(c d e f g)))))

(deftest p19-test
  (testing "Rotate a list N places to the left."
    (is (= (rotate '(a b c d e f g h) 3)
           '(d e f g h a b c)))
    (is (= (rotate '(a b c d e f g h) -2)
           '(g h a b c d e f)))))

(deftest p20-test
  (testing "Remove the K'th element from a list."
    (is (= (remove-at '(a b c d) 2)
           '(a c d)))))

(deftest p21-test
  (testing "Insert an element at a given position into a list."
    (is (= (insert-at 'alfa '(a b c d) 2)
           '(a alfa b c d)))))

(deftest p22-test
  (testing "Create a list containing all integers within a given range."
    (is (= (my-range 4 9)
           '(4 5 6 7 8 9)))
    (is (= (my-range 7 3)
           '(7 6 5 4 3)))))

;; Mocking rand is necessary but I couldn't find proper way in Clojure.
;; Found some interesting techniques in StackOverFlow.
;; https://stackoverflow.com/questions/24007063/how-to-generated-repeatable-random-sequences-with-rand-int
;; (deftest p23-test
;;   (testing "Extract a given number of randomly selected elements from a list."
;;     (is (= (rnd-select '(a b c d e f g h) 3)
;;            '(e d a)))))

(deftest p26-test
  (testing "Generate the combinations of K distinct objects chosen from the N elements of a list"
    (is (= (combination 3 '(a b c d e f))
           '((a b c) (a b d) (a b e) (a b f)
             (a c d) (a c e) (a c f)
             (a d e) (a d f)
             (a e f)
             (b c d) (b c e) (b c f)
             (b d e) (b d f)
             (b e f)
             (c d e) (c d f)
             (c e f)
             (d e f))))))


