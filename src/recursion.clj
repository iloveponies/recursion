(ns recursion)


;; Ex 1
;; Write the function (product coll) that computes the product of a collection of values. The product of aa, bb and cc is a∗b∗ca∗b∗c.

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))


;; Ex 2 (not submitted)
;; Write down the evaluation of (product [1 2 4]) like we did for sum above. This exercise does not give any points and you do not need to return it.


;; Ex 3
;; Write the function (singleton? coll) which returns true if the collection has only one element in it and false otherwise. This is a very useful helper function in the remainder of this chapter.
;; Do not use count as it can be expensive on long sequences. This function is not recursive.

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))


;; Ex 4
;; Write (my-last a-seq) that computes the last element of a sequence.

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))


;; Ex 5
;; Write the function (max-element a-seq) that computes returns the maximum element in a-seq or nil if a-seq is empty?
;; You can use the function (max a b) that returns the greater of a and b.

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))


;; Ex 6
;; Write the function (seq-max seq-1 seq-2) that returns the longer one of seq-1 and seq-2. This is a helper for the next exercise. You do not need to use recursion here. It is okay to use count.

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))


;; Ex 7
;; Write the function (longest-sequence a-seq) that takes a sequence of sequences as a parameter and returns the longest one.

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))


;; Ex 8
;; Implement the function (my-filter pred? a-seq) that works just like the standard filter. Don’t use remove.

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))


;; Ex 9
;; Write the function (sequence-contains? elem a-seq) that returns true if the given sequence contains the given value, otherwise false.

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))


;; Ex 10
;; Write the function (my-take-while pred? a-seq) that returns the longest prefix of a-seq where pred? returns true for every element.

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))


;; Ex 11
;; Write the function (my-drop-while pred? a-seq) that drops elements from a-seq until pred? returns false.

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))


;; Ex 12
;; Write the function (seq= seq-1 seq-2) that compares two sequences for equality.

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq))  false
    (and (= (first a-seq) (first b-seq))) (seq= (rest a-seq) (rest b-seq))
    :else false))


;; Ex 13
;; Write the function (my-map f seq-1 seq-2) that returns a sequence of the following kind . The first item is the return value of f called with the first values of seq-1 and seq-2. The second item is the return value of f called with the second values of seq-1 and seq-2 and so forth until seq-1 or seq-2 ends.
;; This is actually exactly how map works when given two sequences, but for the sake of practice don’t use map when defining my-map.

(defn my-map [f seq-1 seq-2]
  (if
    (or (empty? seq-1) (empty? seq-2)) ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))


;; Ex 14
;; Write the function (power n k) that computes the mathematical expression nknk.

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))


;; Ex 15
;; Compute the nnth Fibonacci number. The nnth Fibonacci number, FnFn, is defined as:
;; F0=0F0=0
;; F1=1F1=1
;; Fn=Fn−1+Fn−2Fn=Fn−1+Fn−2
;; Write the function (fib n) which returns FnFn.

(def fib
  (memoize
    (fn [n]
      (cond
        (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))))


;; Ex 16
;; Write the function (my-repeat how-many-times what-to-repeat) that generates a list with what-to-repeat repeated how-many-times number of times.

(defn my-repeat [how-many-times what-to-repeat]
  (if
    (<= how-many-times 0) ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))


;; Ex 17
;; Write the function (my-range up-to) that works like this:

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))


;; Ex 18
;; Write the functions tails and inits that return all the suffixes and prefixes of a sequence, respectively.

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (apply list a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))


;; Ex 19
;; Write the function (rotations a-seq) that, when given a sequence, returns all the rotations of that sequence.

(defn rotations [a-seq]
  (let [rot1 (fn [s]
               (concat (rest s) (list (first s))))
        rotn (fn rotn [s n]
          (if (<= n 0)
            ()
            (cons s (rotn (rot1 s) (dec n)))))
       ]
    (if (empty? a-seq)
      '(())
      (rotn (seq a-seq) (count a-seq)))))


;; Ex 20
;; Write the function (my-frequencies a-seq) that computes a map of how many times each element occurs in a sequence. E.g.:

(defn my-frequencies-helper [freqs a-seq]
  (let [addone (fn[x]
                   (assoc freqs x (if (contains? freqs x)
                                    (inc (freqs x))
                                    1)))]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper (addone (first a-seq)) (rest a-seq)))))

(defn my-frequencies [a-seq]
 (my-frequencies-helper {} a-seq))


;; Ex 21
;; Write the function (un-frequencies a-map) which takes a map produced by my-frequencies and generates a sequence with the corresponding numbers of different elements.

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (let [[k v] (first a-map)] (repeat v k)) (un-frequencies (rest a-map)))))


;; Ex 22
;; Implement (my-take n coll) that returns n first items of coll.

(defn my-take [n coll]
  (let [my-take-inner (fn my-take-inner [accum n coll]
          (if (or (<= n 0) (empty? coll))
            accum
            (my-take-inner (concat accum (list (first coll))) (dec n) (rest coll))))]
   (my-take-inner '() n coll)))


;; Ex 23
;; Implement (my-drop n coll) that returns all but the n first items of coll.

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))


;; Ex 24
;; Implement the function (halve a-seq) that takes a sequence and returns one vector with two elements. The first element is the first half of a-seq and the second element is the second half of a-seq.
;; To turn a result of division into an integer use int.

(defn halve [a-seq]
  (let [split-seq (fn split-seq [accum a-seq n]
                    (if (or (<= n 0) (empty? a-seq))
                      [accum (seq a-seq)]
                      (split-seq (concat accum (list (first a-seq))) (rest a-seq) (dec n))))]
    (split-seq '() a-seq (quot (count a-seq) 2))))


;; Ex 25
;; Write the function (seq-merge a-seq b-seq) that takes two (low to high) sorted number sequences and combines them into one sorted sequence. Don’t use sort (nor implement it yourself, yet).

(defn seq-merge [a-seq b-seq]
  (let [seq-merge-inner (fn seq-merge-inner [accum a-seq b-seq]
                          (cond
                            (and (empty? a-seq) (empty? b-seq)) (reverse accum)
                            (empty? a-seq) (seq-merge-inner (cons (first b-seq) accum) '() (rest b-seq))
                            (empty? b-seq) (seq-merge-inner (cons (first a-seq) accum) (rest a-seq) '())
                            (= (first a-seq) (first b-seq)) (seq-merge-inner (cons (first b-seq) (cons (first a-seq) accum)) (rest a-seq) (rest b-seq))
                            (< (first a-seq) (first b-seq)) (seq-merge-inner (cons (first a-seq) accum) (rest a-seq) b-seq)
                            :else                           (seq-merge-inner (cons (first b-seq) accum)  a-seq (rest b-seq))
                            ))]
  (seq-merge-inner '() a-seq b-seq)))


;; Ex 26
;; Write the function (merge-sort a-seq) that implements merge sort.
;; The idea of merge sort is to divide the input into subsequences using halve, sort the subsequences recursively and use the seq-merge function to merge the sorted subsequences back together.
;; Conceptually:
;; If the sequence is 0 or 1 elements long, it is already sorted.
;; Otherwise, divide the sequence into two subsequences.
;; Sort each subsequence recursively.
;; Merge the two subsequences back into one sorted sequence.

(defn merge-sort [a-seq]
  (let [[s1 s2] (halve a-seq)]
    (if (or (<= (count s1) 1) (<= (count s2) 1)) ; if either s1 or s2 are 1 in length then the other value will be either 0 or 1 so that's the base case
      (seq-merge s1 s2)
      (seq-merge (merge-sort s1) (merge-sort s2)))))



;; E N C O R E   S E C T I O N  ---  B O N U S   M A R K S   F O R   T H E S E   ! ! !



;; Ex 27
;; 2 points
;; Write the function split-into-monotonics that takes a sequence and returns the sequence split into monotonic pieces

(defn split-into-monotonics [a-seq]
  a-seq)


;; Ex 28
;; 3 points
;; Given a sequence, return all permutations of that sequence.

(defn all-rots [s]
  (->>
    (cycle s)
    (partition (inc (count s)))
    (map #(rest %))
    (take (count s))))

(defn permutations [s]
  (if (empty? (rest s))
    (lazy-seq (list s))
    (for [ars (all-rots s)
          prs (lazy-seq (permutations (rest ars)))]
       (cons (first ars) prs))))


;; Ex 29
;; 3 points
;; Given a set, return the powerset of that set.

(defn powerset [a-set]
  (if (empty? a-set) #{#{}}
    (clojure.set/union (powerset (rest a-set)) (map #(clojure.set/union #{(first a-set)} %) (powerset (rest a-set))))))


