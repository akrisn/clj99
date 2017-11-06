(ns clj99.list)

;;; P01 (*) Find the last box of a list.
;;; Example:
;;; * (my-last '(a b c d))
;;; (D)

;; Actually, built-in function is exist.
;; (defn my-last [lst]
;;   (list (last lst)))

;; You can use `take-last` too.
;; (defn my-last [lst]
;;   (take-last 1 lst))


;; Naive implementation.

;; (defn my-last [lst]
;;   (loop [x lst]
;;     (if (= (count x) 1)
;;       x
;;       (recur (rest x)))))

;; However, this is O(N^2) since count is O(N).


;; O(N) implementation.
(defn my-last [lst]
  (loop [x lst y nil]
    (if (empty? x)
      y
      (recur (rest x) (list (first x))))))

;;; P02 (*) Find the last but one box of a list.
;;; Example:
;;; * (my-but-last '(a b c d))
;;; (C D)

;; Built-in function
;; (defn my-but-last [lst]
;;  (take-last 2 lst))

;; Recursive implementation
(defn my-but-last [lst]
  (loop [x lst ret nil c 0]
    (if (empty? x)
      ret
      (recur (rest x)
             (if (= c 0)
               (list (first x))
               (if (= c 1)
                 (list (first ret) (first x))
                 (list (first (rest ret)) (first x))))
             (inc c)))))

;; The above code constructs intermediate return value and destructs again and again.
;; (take 2 (rev x)) can be more efficient.


;; P03 (*) Find the K'th element of a list.
;; The first element in the list is number 1.
;; Example:
;; * (element-at '(a b c d e) 3)
;; C

;; Built in is nth (0-index).

(defn element-at [lst idx]
  (loop [x lst c idx]
    (if (<= c 1)
      (first x)
      (recur (rest x) (dec c)))))

;; P04 (*) Find the number of elements of a list.

;; Built-in is count.

(defn my-count [lst]
  (loop [x lst c 0]
    (if (empty? x)
      c
      (recur (rest x) (inc c)))))

;; P05 (*) Reverse a list.

;; Built-in is reverse.

(defn my-reverse [lst]
  (loop [x lst ret '()]
    (if (empty? x)
      ret
      (recur (rest x) (cons (first x) ret)))))

;; P06 (*) Find out whether a list is a palindrome.
;; A palindrome can be read forward or backward; e.g. (x a m a x).

(defn palindrome? [lst]
  (= (reverse lst) lst))

;; P07 (**) Flatten a nested list structure.
;; Transform a list, possibly holding lists as elements into a `flat' list
;; by replacing each list with its elements (recursively).
;;
;; Example:
;; * (my-flatten '(a (b (c d) e)))
;; (A B C D E)
;;
;; Hint: Use the predefined functions list and append.

;; Built-in function: flatten

;; (defn my-flatten [lst]
;;   (if (empty? lst)
;;     '()
;;     (let [f (first lst)]
;;       (if (seq? f)
;;         (concat (my-flatten f) (my-flatten (rest lst)))
;;         (cons f (my-flatten (rest lst)))))))

(defn my-flatten [lst]
  (if (seq? lst)
    (reduce concat (map my-flatten lst))
    (list lst)))

;; P08 (**) Eliminate consecutive duplicates of list elements.
;; If a list contains repeated elements they should be replaced with a single copy of the element.
;; The order of the elements should not be changed.
;;
;; Example:
;; * (compress '(a a a a b c c a a d e e e e))
;; (A B C A D E)

(defn compress [lst]
  (let [skip (fn [a x]
               (loop [y x]
                 (if (= (first y) a)
                   (recur (rest y))
                   y)))]
    (if (empty? lst)
      lst
      (cons (first lst) (compress (skip (first lst) (rest lst)))))))

;; P09 (**) Pack consecutive duplicates of list elements into sublists.
;; If a list contains repeated elements they should be placed in separate sublists.
;;
;; Example:
;; * (pack '(a a a a b c c a a d e e e e))
;; ((A A A A) (B) (C C) (A A) (D) (E E E E))

(defn pack [lst]
  (let [pack-sub (fn [x]
                   (loop [a (first x) y (list (first x)) z (rest x)]
                     (if (empty? z)
                       [y z]
                       (if (= (first z) a)
                         (recur a (conj y a) (rest z))
                         [y z]))))]
    (if (empty? lst)
      '()
      (let [[x y] (pack-sub lst)]
        (cons x (pack y))))))

;; P10 (*) Run-length encoding of a list.
;; Use the result of problem P09 to implement the so-called run-length encoding
;; data compression method. Consecutive duplicates of elements are encoded as
;; lists (N E) where N is the number of duplicates of the element E.
;;
;; Example:
;; * (encode '(a a a a b c c a a d e e e e))
;; ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

(defn encode [lst]
  (for [x (pack lst)] (list (count x) (first x))))

;; P11 (*) Modified run-length encoding.
;; Modify the result of problem P10 in such a way that if an element has no duplicates
;; it is simply copied into the result list. Only elements with duplicates are
;; transferred as (N E) lists.
;;
;; Example:
;; * (encode-modified '(a a a a b c c a a d e e e e))
;; ((4 A) B (2 C) (2 A) D (4 E))

(defn encode-modified [lst]
  (for [x (pack lst)]
    (let [c (count x)]
      (if (= c 1)
        (first x)
        (list c (first x))))))

;; P12 (**) Decode a run-length encoded list.
;; Given a run-length code list generated as specified in problem P11.
;; Construct its uncompressed version.

(defn decode [lst]
  (apply concat (map (fn [x]
                       (if (list? x)
                         (repeat (first x) (second x))
                         (list x)))
                     lst)))

;; P13 (**) Run-length encoding of a list (direct solution).
;; Implement the so-called run-length encoding data compression method directly.
;; I.e. don't explicitly create the sublists containing the duplicates, as in
;; problem P09, but only count them. As in problem P11, simplify the result list
;; by replacing the singleton lists (1 X) by X.
;;
;; Example:
;; * (encode-direct '(a a a a b c c a a d e e e e))
;; ((4 A) B (2 C) (2 A) D (4 E))

(defn encode-direct [lst]
  (let [encode-sub (fn [x]
                     (let [a (first x)]
                       (loop [n 1 y (rest x)]
                         (if (empty? y)
                           [(if (= n 1) a (list n a)) y]
                           (if (= (first y) a)
                             (recur (inc n) (rest y))
                             [(if (= n 1) a (list n a)) y])))))]
    (if (empty? lst)
      '()
      (let [[x y] (encode-sub lst)]
        (cons x (encode-direct y))))))

;; P14 (*) Duplicate the elements of a list.
;; Example:
;; * (dupli '(a b c c d))
;; (A A B B C C C C D D)

(defn dupli [lst]
  (if (empty? lst)
    '()
    (let [x (first lst)]
      (cons x (cons x (dupli (rest lst)))))))

;; P15 (**) Replicate the elements of a list a given number of times.
;; Example:
;; * (repli '(a b c) 3)
;; (A A A B B B C C C)

(defn repli [lst n]
  (apply concat (map #(repeat n %) lst)))

;; P16 (**) Drop every N'th element from a list.
;; Example:
;; * (drop '(a b c d e f g h i k) 3)
;; (A B D E G H K)

(defn drop [lst n]
  (for [[a i] (map vector lst (range 1 (inc (count lst)))) :when (not (= (mod i n) 0))] a))

;; P17 (*) Split a list into two parts; the length of the first part is given.
;; Do not use any predefined predicates.
;;
;; Example:
;; * (split '(a b c d e f g h i k) 3)
;; ( (A B C) (D E F G H I K))

(defn split [lst n]
  ((fn split-sub [x n]
     (if (or (empty? x) (= n 0))
       (list '() x)
       (let [[a b] (split-sub (rest x) (dec n))]
         (list (conj a (first x)) b))))
   lst n))

;; P18 (**) Extract a slice from a list.
;; Given two indices, I and K, the slice is the list containing the elements
;; between the I'th and K'th element of the original list (both limits included).
;; Start counting the elements with 1.
;;
;; Example:
;; * (slice '(a b c d e f g h i k) 3 7)
;; (C D E F G)

(defn slice [lst a b]
  ((fn slice-sub1 [lst a b]
     (if (or (empty? lst) (<= a 1))
       ((fn slice-sub2 [lst b]
         (if (or (empty? lst) (= b 0))
           '()
           (conj (slice-sub2 (rest lst) (dec b)) (first lst))))
        lst b)
       (slice-sub1 (rest lst) (dec a) (dec b))))
   lst a b))

;; P19 (**) Rotate a list N places to the left.
;; Examples:
;; * (rotate '(a b c d e f g h) 3)
;; (D E F G H A B C)
;;
;; * (rotate '(a b c d e f g h) -2)
;; (G H A B C D E F)
;;
;; Hint: Use the predefined functions length and append, as well as the result of problem P17.

(defn rotate [lst n]
  (if (> n 0)
    (let [[a b] (split lst n)]
      (concat b a))
    (rotate lst (+ (count lst) n))))

