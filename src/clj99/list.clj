(ns clj99.list)

;;; P01 (*) Find the last box of a list.
;;; Example:
;;; * (my-last '(a b c d))
;;; (D)

;; Actually, built-in function is exist.
;; (defn my-last [coll]
;;   (list (last coll)))

;; You can use `take-last` too.
;; (defn my-last [coll]
;;   (take-last 1 coll))


;; Naive implementation.

;; (defn my-last [coll]
;;   (loop [x coll]
;;     (if (= (count x) 1)
;;       x
;;       (recur (rest x)))))

;; However, this is O(N^2) since count is O(N).

;; O(N) recursive implementation.
(defn my-last [coll]
  (if (next coll)
    (recur (next coll))
    coll))

;;; P02 (*) Find the last but one box of a list.
;;; Example:
;;; * (my-but-last '(a b c d))
;;; (C D)

;; Built-in function
;; (defn my-but-last [coll]
;;  (take-last 2 coll))

;; Recursive implementation
(defn my-but-last [coll]
  (if (nnext coll)
    (recur (next coll))
    coll))

;; P03 (*) Find the K'th element of a list.
;; The first element in the list is number 1.
;; Example:
;; * (element-at '(a b c d e) 3)
;; C

;; Built-in is nth (0-index).

(defn element-at [coll idx]
  (if (<= idx 1)
    (first coll)
    (recur (rest coll) (dec idx))))

;; P04 (*) Find the number of elements of a list.

;; Built-in is count.

(defn my-count [coll]
  (loop [x coll c 0]
    (if (seq x)
      (recur (rest x) (inc c))
      c)))

;; P05 (*) Reverse a list.

;; Built-in is reverse.

(defn my-reverse [coll]
  (loop [x coll ret '()]
    (if (seq x)
      (recur (rest x) (cons (first x) ret))
      ret)))

;; P06 (*) Find out whether a list is a palindrome.
;; A palindrome can be read forward or backward; e.g. (x a m a x).

(defn palindrome? [coll]
  (= (reverse coll) coll))

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

;; (defn my-flatten [coll]
;;   (if (empty? coll)
;;     '()
;;     (let [f (first coll)]
;;       (if (seq? f)
;;         (concat (my-flatten f) (my-flatten (rest coll)))
;;         (cons f (my-flatten (rest coll)))))))

(defn my-flatten [coll]
  (if (seq? coll)
    (mapcat my-flatten coll)
    (list coll)))

;; P08 (**) Eliminate consecutive duplicates of list elements.
;; If a list contains repeated elements they should be replaced with a single copy of the element.
;; The order of the elements should not be changed.
;;
;; Example:
;; * (compress '(a a a a b c c a a d e e e e))
;; (A B C A D E)

(defn compress [coll]
  (let [skip (fn [a x]
               (if (= (first x) a)
                 (recur a (rest x))
                 x))]
    (if (seq coll)
      ;; TODO: use recur
      (cons (first coll) (compress (skip (first coll) (rest coll))))
      coll)))

;; P09 (**) Pack consecutive duplicates of list elements into sublists.
;; If a list contains repeated elements they should be placed in separate sublists.
;;
;; Example:
;; * (pack '(a a a a b c c a a d e e e e))
;; ((A A A A) (B) (C C) (A A) (D) (E E E E))

(defn pack [coll]
  (let [pack-sub (fn [x]
                   (loop [a (first x) y (list (first x)) z (rest x)]
                     (if (empty? z)
                       [y z]
                       (if (= (first z) a)
                         (recur a (conj y a) (rest z))
                         [y z]))))]
    (if (seq coll)
      (let [[x y] (pack-sub coll)]
        ;; use recur
        (cons x (pack y)))
      coll)))

;; P10 (*) Run-length encoding of a list.
;; Use the result of problem P09 to implement the so-called run-length encoding
;; data compression method. Consecutive duplicates of elements are encoded as
;; lists (N E) where N is the number of duplicates of the element E.
;;
;; Example:
;; * (encode '(a a a a b c c a a d e e e e))
;; ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

(defn encode [coll]
  ;; TODO: mapcat?
  (for [x (pack coll)] (list (count x) (first x))))

;; P11 (*) Modified run-length encoding.
;; Modify the result of problem P10 in such a way that if an element has no duplicates
;; it is simply copied into the result list. Only elements with duplicates are
;; transferred as (N E) lists.
;;
;; Example:
;; * (encode-modified '(a a a a b c c a a d e e e e))
;; ((4 A) B (2 C) (2 A) D (4 E))

(defn encode-modified [coll]
  (for [x (pack coll)]
    (let [c (count x)]
      (if (= c 1)
        (first x)
        (list c (first x))))))

;; P12 (**) Decode a run-length encoded list.
;; Given a run-length code list generated as specified in problem P11.
;; Construct its uncompressed version.

(defn decode [coll]
  (apply concat (map (fn [x]
                       (if (list? x)
                         (repeat (first x) (second x))
                         (list x)))
                     coll)))

;; P13 (**) Run-length encoding of a list (direct solution).
;; Implement the so-called run-length encoding data compression method directly.
;; I.e. don't explicitly create the sublists containing the duplicates, as in
;; problem P09, but only count them. As in problem P11, simplify the result list
;; by replacing the singleton lists (1 X) by X.
;;
;; Example:
;; * (encode-direct '(a a a a b c c a a d e e e e))
;; ((4 A) B (2 C) (2 A) D (4 E))

(defn encode-direct [coll]
  (let [encode-sub (fn [x]
                     (let [a (first x)]
                       (loop [n 1 y (rest x)]
                         (if (seq y)
                           (if (= (first y) a)
                             (recur (inc n) (rest y))
                             [(if (= n 1) a (list n a)) y])
                           [(if (= n 1) a (list n a)) y]))))]
    (if (seq coll)
      (let [[x y] (encode-sub coll)]
        (cons x (encode-direct y)))
      coll)))

;; P14 (*) Duplicate the elements of a list.
;; Example:
;; * (dupli '(a b c c d))
;; (A A B B C C C C D D)

(defn dupli [coll]
  (if (seq coll)
    (let [x (first coll)]
      (cons x (cons x (dupli (rest coll)))))
    coll))

;; P15 (**) Replicate the elements of a list a given number of times.
;; Example:
;; * (repli '(a b c) 3)
;; (A A A B B B C C C)

(defn repli [coll n]
  (apply concat (map #(repeat n %) coll)))

;; P16 (**) Drop every N'th element from a list.
;; Example:
;; * (drop '(a b c d e f g h i k) 3)
;; (A B D E G H K)

(defn drop [coll n]
  (for [[a i] (map vector coll (range 1 (inc (count coll)))) :when (not (= (mod i n) 0))] a))

;; P17 (*) Split a list into two parts; the length of the first part is given.
;; Do not use any predefined predicates.
;;
;; Example:
;; * (split '(a b c d e f g h i k) 3)
;; ( (A B C) (D E F G H I K))

(defn split [coll n]
  ((fn split-sub [x n]
     (if (and (seq x) (> n 0))
       (let [[a b] (split-sub (rest x) (dec n))]
         (list (conj a (first x)) b))
       (list '() x)))
   coll n))

;; P18 (**) Extract a slice from a list.
;; Given two indices, I and K, the slice is the list containing the elements
;; between the I'th and K'th element of the original list (both limits included).
;; Start counting the elements with 1.
;;
;; Example:
;; * (slice '(a b c d e f g h i k) 3 7)
;; (C D E F G)

(defn slice [coll a b]
  ((fn slice-sub1 [coll a b]
     (if (and (seq coll) (> a 1))
       (slice-sub1 (rest coll) (dec a) (dec b))
       ((fn slice-sub2 [coll b]
         (if (or (empty? coll) (= b 0))
           '()
           (conj (slice-sub2 (rest coll) (dec b)) (first coll))))
        coll b)))
   coll a b))

;; P19 (**) Rotate a list N places to the left.
;; Examples:
;; * (rotate '(a b c d e f g h) 3)
;; (D E F G H A B C)
;;
;; * (rotate '(a b c d e f g h) -2)
;; (G H A B C D E F)
;;
;; Hint: Use the predefined functions length and append, as well as the result of problem P17.

(defn rotate [coll n]
  (if (> n 0)
    (let [[a b] (split coll n)]
      (concat b a))
    (rotate coll (+ (count coll) n))))

;; P20 (*) Remove the K'th element from a list.
;; Example:
;; * (remove-at '(a b c d) 2)
;; (A C D)

(defn remove-at [coll n]
  (apply concat ((fn remove-at-sub [x n]
                   (if (and (seq x) (> n 1))
                     (let [[a b] (remove-at-sub (rest x) (dec n))]
                       (list (conj a (first x)) b))
                     (list '() (rest x))))
                 coll n)))

;; P21 (*) Insert an element at a given position into a list.
;; Example:
;; * (insert-at 'alfa '(a b c d) 2)
;; (A ALFA B C D)

(defn insert-at [elem coll n]
  ((fn insert-at-sub [e x n]
     (if (and (seq x) (> n 1))
       (let [y (insert-at-sub e (rest x) (dec n))]
         (conj y (first x)))
       (conj x e)))
   elem coll n))

;; P22 (*) Create a list containing all integers within a given range.
;; If first argument is smaller than second, produce a list in decreasing order.
;; Example:
;; * (range 4 9)
;; (4 5 6 7 8 9)

;; There is a built-in function.
;;(defn my-range [a b]
;;  (range a (inc b)))

(defn my-range [a b]
  (if (= a b)
    (list a)
    (if (< a b)
      (conj (my-range (inc a) b) a)
      (conj (my-range (dec a) b) a))))

;; P23 (**) Extract a given number of randomly selected elements from a list.
;; The selected items shall be returned in a list.
;; Example:
;; * (rnd-select '(a b c d e f g h) 3)
;; (E D A)
;;
;; Hint: Use the built-in random number generator and the result of problem P20.

(defn rnd-select [coll n]
  (if (= n 0)
    nil
    (let [r (inc (rand-int (my-count coll)))]
      (conj (rnd-select (remove-at coll r) (dec n))
            (element-at coll r)))))


;; P24 (*) Lotto: Draw N different random numbers from the set 1..M.
;; The selected numbers shall be returned in a list.
;; Example:
;; * (lotto-select 6 49)
;; (23 1 17 33 21 37)
;;
;; Hint: Combine the solutions of problems P22 and P23.

(defn lotto-select [n m]
  (rnd-select (my-range 1 m) n))


;; P25 (*) Generate a random permutation of the elements of a list.
;; Example:
;; * (rnd-permu '(a b c d e f))
;; (B A D C E F)
;;
;; Hint: Use the solution of problem P23.

(defn rnd-permu [coll]
  (rnd-select coll (my-count coll)))


;; P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
;; In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that
;; there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients).
;; For pure mathematicians, this result may be great. But we want to really generate all the
;; possibilities in a list.
;;
;; Example:
;; * (combination 3 '(a b c d e f))
;; ((A B C) (A B D) (A B E) ... )

(defn combination [n coll]
  (if (or (= n 0) (< (count coll) n))
    nil
    (if (= n 1)
      (map (fn [x] (list x)) coll)
      (let [ret-for-skip (combination n (rest coll))
            ret-with-first (combination (dec n) (rest coll))]
        (concat (map (fn [x] (conj x (first coll))) ret-with-first)
                ret-for-skip)))))
