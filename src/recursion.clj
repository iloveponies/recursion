(ns recursion)

;Ex1 Write the function (product coll) that computes the product of a collection of values. The product of aa, bb and cc is a∗b∗ca∗b∗c.
(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

;Ex2 Write down the evaluation of (product [1 2 4]) like we did for sum above. This exercise does not give any points and you do not need to return it.
#_(
    (product [1 2 3])
    = (product (conj (conj (conj [] 1) 2) 3))
    => (* 3 (product (conj (conj [] 1) 2)))
    => (* 3 (* 2 (product (conj [] 1))))
    => (* 3 (* 2 (* 1 (product []))))                       ; (empty? []) is true, so (product [] ;=> 1
    => (* 3 (* 2 (* 1 1)))
    => (* 3 (* 2 1))
    => (* 3 2)
    => 6
    )

;Ex3 Write the function (singleton? coll) which returns true if the collection has only one element in it and false otherwise.
; This is a very useful helper function in the remainder of this chapter.
;Do not use count as it can be expensive on long sequences. This function is not recursive.
(defn singleton? [coll]
  (and
    (boolean (not-empty coll))
    (empty? (rest coll))
    ))


;Ex4 Write (my-last a-seq) that computes the last element of a sequence.
(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

;Ex5 Write the function (max-element a-seq) that computes returns the maximum element in a-seq or nil if a-seq is empty?
;You can use the function (max a b) that returns the greater of a and b.
(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq)
               (max-element (rest a-seq)))))

;Ex6 Write the function (seq-max seq-1 seq-2) that returns the longer one of seq-1 and seq-2.
; This is a helper for the next exercise. You do not need to use recursion here. It is okay to use count.
(defn seq-max [seq-1 seq-2]
  (let [cnt1 (count seq-1)
        cnt2 (count seq-2)]
    (if (= cnt2 (max-element [cnt1 cnt2]))
      seq-2
      seq-1)))

;Ex7 Write the function (longest-sequence a-seq) that takes a sequence of sequences as a parameter and returns the longest one.
(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    (= (count a-seq) 2) (seq-max (first a-seq) (second a-seq))
    :else (longest-sequence (rest a-seq))))

;Ex8 Implement the function (my-filter pred? a-seq) that works just like the standard filter. Don’t use remove.
(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))
;Ex9 Write the function (sequence-contains? elem a-seq) that returns true if the given sequence contains the given value, otherwise false.
(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

;Ex10 Write the function (my-take-while pred? a-seq) that returns the longest prefix of a-seq where pred? returns true for every element.
(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

;Ex11 Write the function (my-drop-while pred? a-seq) that drops elements from a-seq until pred? returns false.
(defn my-drop-while [pred? a-seq]
  #_(cond
      (empty? a-seq) (sequence a-seq)
      (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
      :else (sequence a-seq))
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq)

  )

;Ex12 Write the function (seq= seq-1 seq-2) that compares two sequences for equality.
(defn seq= [a-seq b-seq]
  (cond
    (not= (count a-seq) (count b-seq)) false
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

;Ex13 Write the function (my-map f seq-1 seq-2) that returns a sequence of the following kind.
; The first item is the return value of f called with the first values of seq-1 and seq-2.
; The second item is the return value of f called with the second values of seq-1 and seq-2 and so forth until seq-1 or seq-2 ends.
;This is actually exactly how map works when given two sequences, but for the sake of practice don’t use map when defining my-map.
(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2)))))

;Ex14 Write the function (power n k) that computes the mathematical expression n^k.
(defn power [n k]
  ;(apply * (repeat k n))
  (if (zero? k)
    1
    (* n (power n (dec k)))))

;Ex15 Write the function (fib n) which returns Fn.
(defn fib [n]
  (case n
    0 0
    1 1
    (+ (fib (dec n)) (fib (- n 2)))))

;Ex16 Write the function (my-repeat how-many-times what-to-repeat) that generates a list with what-to-repeat repeated how-many-times number of times.
(defn my-repeat [how-many-times what-to-repeat]
  (if (or (zero? how-many-times) (neg? how-many-times))
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

;Ex17 Write the function (my-range up-to) that works like this:
#_((my-range 0)                                             ;=> ()
    (my-range 1)                                            ;=> (0)
    (my-range 2)                                            ;=> (1 0)
    (my-range 3)                                            ;=> (2 1 0)
    )
(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (conj (my-range (dec up-to)) (dec up-to))))

;Ex18 Write the functions tails and inits that return all the suffixes and prefixes of a sequence, respectively.
(defn tails [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (reverse (map reverse (tails (reverse a-seq))))))

;Ex19 Write the function (rotations a-seq) that, when given a sequence, returns all the rotations of that sequence.
(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

;Ex20 Write the function (my-frequencies a-seq) that computes a map of how many times each element occurs in a sequence. E.g.:
(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (find freqs (first a-seq))
      (my-frequencies-helper (update-in freqs [(first a-seq)] inc) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

;Ex21 Write the function (un-frequencies a-map) which takes a map produced by
; my-frequencies and generates a sequence with the corresponding numbers of different elements.
(defn un-frequencies [a-map]
  (flatten (map repeat (vals a-map) (keys a-map))))

;Ex22 Implement (my-take n coll) that returns n first items of coll.
(defn my-take [n coll]
  (cond
    (> n (count coll)) coll
    (empty? coll) coll
    (= n 0) '()
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

;Ex33 Implement (my-drop n coll) that returns all but the n first items of coll.
(defn my-drop [n coll]
  (cond
    (>= n (count coll)) '()
    (empty? coll) coll
    (= n 0) coll
    :else (cons (get coll n) (my-drop (inc n) coll))))



;Ex24 Implement the function (halve a-seq) that takes a sequence and returns one vector with two elements.
;The first element is the first half of a-seq and the second element is the second half of a-seq.
(defn halve [a-seq]
  (let [half-len (int (/ (count a-seq) 2))
        first-part (into [] (my-take half-len a-seq))
        second-part (into [] (my-drop half-len a-seq))]
    (vector first-part second-part)))

;Write the function (seq-merge a-seq b-seq) that takes two (low to high) sorted number sequences and combines them into one sorted sequence.
; Don’t use sort (nor implement it yourself, yet).

(defn seq-merge-helper [a-seq b-seq sorted-seq]
  (let [[a _] a-seq
        [b _] b-seq]
    (cond
      (empty? a-seq) (concat sorted-seq b-seq)
      (empty? b-seq) (concat sorted-seq a-seq)
      (< a b) (seq-merge-helper (rest a-seq) b-seq (concat sorted-seq [a]))
      :else (seq-merge-helper a-seq (rest b-seq) (concat sorted-seq [b])))))


(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq '()))


;Ex26 Write the function (merge-sort a-seq) that implements merge sort.
;The idea of merge sort is to divide the input into subsequences using halve,
; sort the subsequences recursively and use the seq-merge function to merge the sorted subsequences back together.
(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[a b] (into [] (halve a-seq))]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

