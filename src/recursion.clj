(ns recursion)

(defn sum [coll]
  (if (empty? coll)
    0
    (+ (first coll)
       (sum (rest coll)))))

;  Write the function (product coll) that computes the product of a collection
; of values. The product of a, b and c is a*b*c.
;  (product [])        ;=> 1  ; special case
;  (product [1 2 3])   ;=> 6
;  (product [1 2 3 4]) ;=> 24
;  (product [0 1 2])   ;=> 0
;  (product #{2 3 4})  ;=> 24 ; works for sets too!
(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

;  Write the function (singleton? coll) which returns true if the collection
;  has only one element in it and false otherwise. This is a very useful helper
;  function in the remainder of this chapter.
;  Do not use count as it can be expensive on long sequences.
;  This function is not recursive.
;  (singleton? [1])     ;=> true
;  (singleton? #{2})    ;=> true
;  (singleton? [])      ;=> false
;  (singleton? [1 2 3]) ;=> false
(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll))))

; Write (my-last a-seq) that computes the last element of a sequence.
; (my-last [])      ;=> nil
; (my-last [1 2 3]) ;=> 3
; (my-last [2 5])   ;=> 5
; Hint: what is the base case here? How can you check if you’re there?
(defn my-last [coll]
  (cond
    (empty? coll) nil
    (empty? (rest coll)) (first coll)
    :else (my-last (rest coll))))

; Write the function (max-element a-seq) that computes returns the maximum
; element in a-seq or nil if a-seq is empty?
; You can use the function (max a b) that returns the greater of a and b.
; (max-element [2 4 1 4]) ;=> 4
; (max-element [2])       ;=> 2
; (max-element [])        ;=> nil
(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq)
               (max-element (rest a-seq)))))

;  Write the function (seq-max seq-1 seq-2) that returns the longer one of
; seq-1 and seq-2. This is a helper for the next exercise. You do not need to
; use recursion here. It is okay to use count.
;  (seq-max [1] [1 2])   ;=> [1 2]
;  (seq-max [1 2] [3 4]) ;=> [3 4]
(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

; Write the function (longest-sequence a-seq) that takes a sequence of sequences
; as a parameter and returns the longest one.
; (longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
; (longest-sequence [[1 2]])            ;=> [1 2]
; (longest-sequence [])                 ;=> nil
(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max
            (first a-seq)
            (longest-sequence (rest a-seq)))))


; Implement the function (my-filter pred? a-seq) that works just like the
; standard filter. Don’t use remove.
; (my-filter odd? [1 2 3 4]) ;=> (1 3)
; (my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
; (my-filter even? [1 3 5 7]) ;=> ()
(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [first-item (first a-seq)
          first-passes? (pred? first-item)
          rest-filtered (my-filter pred? (rest a-seq))]
         (if first-passes?
           (conj rest-filtered first-item)
           rest-filtered))))

;  Write the function (sequence-contains? elem a-seq) that returns true if
;  the given sequence contains the given value, otherwise false.
;  (sequence-contains? 3 [1 2 3]) ;=> true
;  (sequence-contains? 3 [4 7 9]) ;=> false
;  (sequence-contains? :pony [])  ;=> false
;  Hint: remember to stop searching when you find it.))))
(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

; Write the function (my-take-while pred? a-seq) that returns the longest
; prefix of a-seq where pred? returns true for every element.)
; (my-take-while odd? [1 2 3 4])  ;=> (1)
; (my-take-while odd? [1 3 4 5])  ;=> (1 3)
; (my-take-while even? [1 3 4 5]) ;=> ()
; (my-take-while odd? [])         ;=> ()
(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (conj (my-take-while pred? (rest a-seq))
                                (first a-seq))
    :else '()))

; Write the function (my-drop-while pred? a-seq) that drops elements from
; a-seq until pred? returns false.
; (my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
; (my-drop-while odd? [1 3 4 5])  ;=> (4 5)
; (my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
; (my-drop-while odd? [])         ;=> ()
(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (seq a-seq)))

; Write the function (seq= seq-1 seq-2) that compares two sequences for
; equality.
; (seq= [1 2 4] '(1 2 4))  ;=> true
; (seq= [1 2 3] [1 2 3 4]) ;=> false
; (seq= [1 3 5] [])        ;=> false
(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (empty? a-seq) false
    (empty? b-seq) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

; Write the function (my-map f seq-1 seq-2) that returns a sequence of the
; following kind . The first item is the return value of f called with the first
; values of seq-1 and seq-2. The second item is the return value of f called
; with the second values of seq-1 and seq-2 and so forth until seq-1 or seq-2
; ends.
; This is actually exactly how map works when given two sequences, but for the
; sake of practice don’t use map when defining my-map.
; (my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)
; (my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
; (my-map + [1 2 3] [])        ;=> ()
(defn my-map [f seq-1 seq-2]
  (if
    (or (empty? seq-1) (empty? seq-2))
    '()
    (conj
      (my-map f (rest seq-1) (rest seq-2))
      (f (first seq-1) (first seq-2)))))

; Write the function (power n k) that computes the mathematical expression n^k.
; (power 2 2)  ;=> 4
; (power 5 3)  ;=> 125
; (power 7 0)  ;=> 1
; (power 0 10) ;=> 0
(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))


; Compute the nnth Fibonacci number. The nnth Fibonacci number, FnFn, is defined as:
; F0 = 0
; F1 = 1
; Fn = Fn-1 + Fn-2
; Write the function (fib n) which returns Fn.
; (fib 0) ;=> 0
; (fib 1) ;=> 1
; (fib 2) ;=> 1
; (fib 3) ;=> 2
; (fib 4) ;=> 3
; (fib 5) ;=> 5
; (fib 6) ;=> 8
; ...
; (fib 10) ;=> 55
(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1))
             (fib (- n 2)))))

;Write the function (my-repeat how-many-times what-to-repeat) that generates a list
;with what-to-repeat repeated how-many-times number of times.
;(my-repeat 2 :a)    ;=> (:a :a)
;(my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
;(my-repeat -1 :a)   ;=> ()
(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (conj (my-repeat (dec how-many-times) what-to-repeat)
          what-to-repeat)))

;; Write the function (my-range up-to) that works like this:
;; (my-range 0)  ;=> ()
;; (my-range 1)  ;=> (0)
;; (my-range 2)  ;=> (1 0)
;; (my-range 3)  ;=> (2 1 0)
(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (conj (my-range (dec up-to))
          (dec up-to))))

;; Write the functions tails and inits that return all the suffixes and prefixes of a sequence, respectively.
;; (tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())
;; (inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))
;; ; You can output the tails and inits in any order you like.
;; (inits [1 2 3 4]) ;=> ((1 2) () (1 2 3) (1) (1 2 3 4))
;; Hint: You can use reverse and map.
;; (reverse [1 2 3]) ;=> (3 2 1)
;; (reverse [2 3 1]) ;=> (1 3 2)
(defn tails [a-seq]
  (if (empty? a-seq)
    (conj a-seq '())
    (conj (tails (rest a-seq)) a-seq)))

(defn inits [a-seq]
  (if (empty? a-seq)
    (conj a-seq '())
    (conj (inits (butlast a-seq)) a-seq)))

;; Write the function (rotations a-seq) that, when given a sequence, returns all the rotations of that sequence.
;; (rotations [])        ;=> (())
;; (rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
;; (rotations [:a :b])   ;=> ((:a :b) (:b :a))
;; ; The order of rotations does not matter.
;; (rotations [:a :b])   ;=> ((:b :a) (:a :b))
;; (rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
;; (count (rotations [6 5 8 9 2])) ;=> 5
;; Keep in mind the function concat.
;; (concat [1 2 3] [:a :b :c]) ;=> (1 2 3 :a :b :c)
;; (concat [1 2] [3 4 5 6])    ;=> (1 2 3 4 5 6)
(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (let [window-size (count a-seq)
          step 1
          collection (rest (concat a-seq a-seq))]
      (partition window-size step collection))))

;; Write the function (my-frequencies a-seq) that computes a map of how many times each element occurs in a sequence. E.g.:
;; (my-frequencies []) ;=> {}
;; (my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}
(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [element (first a-seq)
          old-count (or (get freqs element) 0)
          new-count (inc old-count)
          new-freqs (assoc freqs element new-count)]
        (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


;; Write the function (un-frequencies a-map) which takes a map produced by my-frequencies and
;; generates a sequence with the corresponding numbers of different elements.
;; (un-frequencies {:a 3 :b 2 "^_^" 1})             ;=> (:a :a :a "^_^" :b :b)
;; (un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
;; (my-frequencies (un-frequencies {:a 100 :b 10})) ;=> {:a 100 :b 10}
;; The order of elements in the output sequence doesn’t matter.
;; Hint 1: Remember that you can use first and rest on a map too!
;; (first {:a 1 :b 2}) ;=> [:a 1]
;; (rest {:a 1 :b 2 :c 3}) ;=> ([:b 2] [:c 3])
;; Hint 2: There are multiple ways to implement this, but consider using concat and repeat.
(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [pair (first a-map)
          values (repeat (val pair) (key pair))]
      (concat values
              (un-frequencies (rest a-map))))))

;; Implement (my-take n coll) that returns n first items of coll.
;; (my-take 2 [1 2 3 4]) ;=> (1 2)
;; (my-take 4 [:a :b])   ;=> (:a :b)
(defn my-take [n coll]
  (cond (empty? coll) '()
        (<= n 0) '()
        :else (conj
                (my-take (dec n) (rest coll))
                (first coll))))

;; Implement (my-drop n coll) that returns all but the n first items of coll.
;; (my-drop 2 [1 2 3 4]) ;=> (3 4)
;; (my-drop 4 [:a :b])   ;=> ()
(defn my-drop [n coll]
  (cond (empty? coll) '()
        (<= n 0) coll
        :else (my-drop (dec n) (rest coll))))


;; Implement the function (halve a-seq) that takes a sequence and returns one vector with two elements.
;; The first element is the first half of a-seq and the second element is the second half of a-seq.
;; To turn a result of division into an integer use int.
;; (int (/ 7 2)) ;=> 3
;; (halve [1 2 3 4])   ;=> [(1 2) (3 4)]
;; (halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
;; (halve [1])         ;=> [() (1)]
(defn halve [a-seq]
  (let [length (count a-seq)
        length-halved (int (/ length 2))]
    (cond
      (= 0 length) '()
      :else [(my-take length-halved a-seq)
             (my-drop length-halved a-seq)])))

;; Write the function (seq-merge a-seq b-seq) that takes two (low to high) sorted number sequences
;; and combines them into one sorted sequence. Don’t use sort (nor implement it yourself, yet).
;; (seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
;; (seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)
(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq))
    (concat [(first a-seq)] (seq-merge (rest a-seq) b-seq))
    :else
    (concat [(first b-seq)] (seq-merge a-seq (rest b-seq)))))

;; Write the function (merge-sort a-seq) that implements merge sort.
;; The idea of merge sort is to divide the input into subsequences using halve,
;; sort the subsequences recursively and use the seq-merge function to merge the
;; sorted subsequences back together.
;; Conceptually:
;; If the sequence is 0 or 1 elements long, it is already sorted.
;; Otherwise, divide the sequence into two subsequences.
;; Sort each subsequence recursively.
;; Merge the two subsequences back into one sorted sequence.
;;     (merge-sort [4 2 3 1])
;; ;=> (seq-merge (merge-sort (4 2))
;; ;              (merge-sort (3 1)))
;; ;=> (seq-merge (seq-merge (merge-sort (4))
;; ;                         (merge-sort (2)))
;; ;              (seq-merge (merge-sort (3))
;; ;                         (merge-sort (1))))
;; ;=> (seq-merge (seq-merge (4) (2))
;; ;              (seq-merge (3) (1)))
;; ;=> (seq-merge (2 4) (1 3))
;; ;=> (1 2 3 4)
;; (merge-sort [])                 ;=> ()
;; (merge-sort [1 2 3])            ;=> (1 2 3)
;; (merge-sort [5 3 4 17 2 100 1]) ;=> (1 2 3 4 5 17 100)
(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [halves (halve a-seq)
          first-half (get halves 0)
          second-half (get halves 1)]
      (seq-merge (merge-sort first-half)
                 (merge-sort second-half)))))

;; Write the function split-into-monotonics that takes a sequence and returns the sequence
;; split into monotonic pieces. Examples:
;; (split-into-monotonics [0 1 2 1 0])   ;=> ((0 1 2) (1 0))
;; (split-into-monotonics [0 5 4 7 1 3]) ;=> ((0 5) (4 7) (1 3))
;; Hint: You might find useful the functions take-while, drop and inits. Make sure that your
;; inits returns the prefixes from the shortest to the longest.

(defn monotonic? [a-seq]
  (or (empty? a-seq)
      (apply < a-seq)
      (apply > a-seq)))

(defn monotonics-helper [a-seq result]
  (if (empty? a-seq)
    result
    (let [initials-reversed (reverse (inits a-seq))
          monotonic-sequences (take-while monotonic? initials-reversed)
          monotonic (last monotonic-sequences)]
      (monotonics-helper (drop (count monotonic) a-seq) (conj result monotonic)))))

(defn split-into-monotonics [a-seq]
  (monotonics-helper a-seq []))

;; 3 points
;; Given a sequence, return all permutations of that sequence.
;; (permutations #{})
;; ;=> (())
;; (permutations #{1 5 3})
;; ;=> ((1 5 3) (5 1 3) (5 3 1) (1 3 5) (3 1 5) (3 5 1))
;; The order of the permutations doesn’t matter.


;; TODO is there a better way to do this?
(defn insert-to-pos [a-seq pos element]
  (let [prefix (take pos a-seq)
        postfix (drop pos a-seq)]
    (concat prefix [element] postfix)))

(defn insert-to-all-positions [a-seq element]
  (let [positions (range 0 (+ (count a-seq) 1))
        insert-fn (fn [pos] (insert-to-pos a-seq pos element))]
    (map insert-fn positions)))

;; https://en.wikipedia.org/wiki/Permutation#Software_implementations
;; Insert into all positions solution
(defn permute [input]
  (if (<= (count input) 1)
    [input]
    (let [to-insert (first input)
          permutations (permute (rest input))
          insert-fn (fn [permutation] (insert-to-all-positions permutation to-insert))]
      (mapcat insert-fn permutations))))

(defn permutations [a-set]
  (permute (into [] a-set)))

;; Given a set, return the powerset of that set.
;; (powerset #{})      ;=> #{#{}}
;; (powerset #{1 2 4}) ;=> #{#{} #{4} #{2} #{2 4} #{1} #{1 4} #{1 2} #{1 2 4}}
(defn powerset [a-seq]
  (let [a-set (set a-seq)]
    (set
      (conj
        (mapcat
          (fn [element] (powerset (disj a-set element)))
          a-set)
        a-set))))
