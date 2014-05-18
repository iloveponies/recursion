(ns recursion)



;; Exercise 1
;; Write the function (product coll) that computes the product of a collection of values. The product of a, b and c is a∗b∗c.
;;
;; coll -> number
;; naive version
(defn product [coll]
  (if (empty? coll)
    ;; base case
    1
    ;; recursion
    (* (first coll)
       (product (rest coll)))))
;;
;; loop macro version
(defn product [coll]
  (loop [c    coll  ; temporary variable holding collection
         acc     1] ; temporary variable serving as an accumulator

    (if (empty? c)
      ;; base case
      acc
      ;; recursion
      (recur (* acc (first c)) (rest c)))))
;;
;; two arity version with tail call optimization without loop macro
(defn product
  ;; body with 1 arg (collection only)
  ([coll] (product coll 1))
  ;;
  ;; body with 2 args (collection and an accumulator)
  ([coll acc]
     (if (empty? coll)
       ;; base case
       acc
       ;; recursion
       (recur (rest coll) (* acc (first coll))))))
;;
;; (product [])        ;=> 1  ; special case
;; (product [1 2 3])   ;=> 6
;; (product [1 2 3 4]) ;=> 24
;; (product [0 1 2])   ;=> 0
;; (product #{2 3 4})  ;=> 24 ; works for sets too!


;; Exercise 2
;; Write down the evaluation of (product [1 2 4]) like we did for sum above. This exercise does not give any points and you do not need to return it.
(* 1 (* 2 (* 4 1)))


;; Exercise 3
;; Write the function (singleton? coll) which returns true if the collection has only one element in it and false otherwise. This is a very useful helper function in the remainder of this chapter.
;; Do not use count as it can be expensive on long sequences. This function is not recursive.
;;
;; collection -> bool
;; check if there is only one element without count
(defn singleton? [coll]
  (and ;; both conditions are required
   ;; Overall not empty
   (not (empty? coll))
   ;; Rest of the collection is empty
   (empty? (rest coll))))
;;
(singleton? [1])     ;=> true
(singleton? #{2})    ;=> true
(singleton? [])      ;=> false
(singleton? [1 2 3]) ;=> false
(singleton? [nil]) ;=> true
(singleton? [nil nil]) ;=> false



;; Exercise 4
;; Write (my-last a-seq) that computes the last element of a sequence.
;; Hint: what is the base case here? How can you check if you’re there?
;;
;; coll -> bool
;; get the last elmenent, nil if empty
;;
;; naive
;; (defn my-last [coll]
;;   ;; first check for emptiness
;;   (if (empty? coll)
;;     nil
;;     ;; if non-empty use singleton?
;;     (if (singleton? coll)
;;       (first coll)
;;       (recur (rest coll)))))
;; it works on [nil] but probably not safe
;;
;; multi-arity tail call optimization version
(defn my-last
  ([coll] (my-last (rest coll) (first coll)))
  ([coll acc]
     (if (empty? coll)
       acc
       (recur (rest coll) (first coll)))))
;;
;; (my-last [])      ;=> nil
;; (my-last [1 2 3]) ;=> 3
;; (my-last [2 5])   ;=> 5



;; Exercise 5
;; Write the function (max-element a-seq) that computes returns the maximum element in a-seq or nil if a-seq is empty?
;; You can use the function (max a b) that returns the greater of a and b.
;;
;; coll -> number
;; return maximum value in the collection, nil if empty.
;;
;; reduce solution
(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (reduce max a-seq)))
;;
;; (max-element [2 4 1 4]) ;=> 4
;; (max-element [2])       ;=> 2
;; (max-element [])        ;=> nil


;; Exercise 6
;; Write the function (seq-max seq-1 seq-2) that returns the longer one of seq-1 and seq-2. This is a helper for the next exercise. You do not need to use recursion here. It is okay to use count.
(defn seq-max [seq-1 seq-2]
  (if (>= (count seq-2) (count seq-1))
    seq-2
    seq-1))
;;
;; (seq-max [1] [1 2])   ;=> [1 2]
;; (seq-max [1 2] [3 4]) ;=> [3 4]



;; Exercise 7
;; Write the function (longest-sequence a-seq) that takes a sequence of sequences as a parameter and returns the longest one.
;;
;; coll of coll -> coll
;;
(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (reduce seq-max a-seq)))
;;
;; (longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
;; (longest-sequence [[1 2]])            ;=> [1 2]
;; (longest-sequence [])                 ;=> nil



;; Exercise 8
;; Implement the function (my-filter pred? a-seq) that works just like the standard filter. Don’t use remove.
;;
;; coll -> coll
;; keep ones meeting criteria
;;
;; naive (no tail call optimization)
(defn my-filter [pred? a-seq]

  (if (empty? a-seq)
    ;; if empty, return empty coll
    a-seq

    ;; if not, check if the first element meets pred?
    (if (pred? (first a-seq))
      ;; if met, add it
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))

      ;; if not met, skip it
      (my-filter pred? (rest a-seq)))))
;;
;; multi-arity version with tail-call optimization
(defn my-filter
  ;; body 1 with 2 args
  ([pred? a-seq]
     (if (empty? a-seq)
       ;; if empty, return it and end
       a-seq
       ;; if not, start the accumulator with an empty collection
       (my-filter pred? a-seq [])))
  ;;
  ;; body 2 with 3 args including the accumulator
  ([pred? a-seq acc]
     (if (empty? a-seq)
       ;; if done, return acc
       acc
       ;; otherwise, assess the first element
       (if (pred? (first a-seq))
         ;; if met, add to the accumulator and recur
         (recur pred? (rest a-seq) (conj acc (first a-seq)))
         ;; if not met, recur without accumulating
         (recur pred? (rest a-seq) acc)))))
;;
;; loop macro version with tail-call optimization
(defn my-filter [pred? a-seq]
  (loop [sq a-seq
         acc []]
    (if (empty? sq)
      ;; if nothing left, return the accumulator
      acc
      ;; otherwise recur.
      (recur (rest sq) (if (pred? (first sq))
                         ;; if the condition is met, add the element to acc
                         (conj acc (first sq))
                         ;; Otherwise, pass this iteration without change
                         acc)))))
;;
;; (my-filter odd? [1 2 3 4]) ;=> (1 3)
;; (my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
;; (my-filter even? [1 3 5 7]) ;=> ()


;; Stopping before the end
;; Sometimes you can find the answer before hitting the base case. For example, the following function checks if a sequence contains only numbers. If we find something that isn’t a number on the way through, we can immediately stop and return false.
;;
;; (defn only-numbers? [coll]
;;   (cond
;;    (empty? coll) true                                 ; the empty sequence contains only numbers
;;    (number? (first coll)) (only-numbers? (rest coll)) ; we got a number, let's check the rest
;;    :else false))                                      ; it wasn't a number so we have our answer
;; Two stop rules by using cond instead of if


;; Exercise 9
;; Write the function (sequence-contains? elem a-seq) that returns true if the given sequence contains the given value, otherwise false.
;; Hint: remember to stop searching when you find it.
(defn sequence-contains? [elem a-seq]
  (cond
   ;; Two stop coditions
   ;; Stop at the end
   (empty? a-seq) false
   ;; Stop by findign a match
   (= elem (first a-seq)) true
   ;; If not found and non empty, recur
   :else (recur elem (rest a-seq))))
;;
;; (sequence-contains? 3 [1 2 3]) ;=> true
;; (sequence-contains? 3 [4 7 9]) ;=> false
;; (sequence-contains? :pony [])  ;=> false



;; Exercise 10
;; Write the function (my-take-while pred? a-seq) that returns the longest prefix of a-seq where pred? returns true for every element.
;;
;; coll -> coll
;; stop if empty or first false
;;
;; naive
(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)               '()
   (not (pred? (first a-seq)))  '()
   :else (cons (first a-seq)    (my-take-while pred? (rest a-seq)))))
;;
;; multi-arity version
(defn my-take-while
  ;; body 1 with 2 args. provide an empty acc
  ([pred? a-seq] (my-take-while pred? a-seq []))
  ;;
  ;; body 2 with 3 args including acc
  ([pred? a-seq acc]
     (cond
      ;; two stop conditions
      (empty? a-seq) acc
      (not (pred? (first a-seq))) acc
      ;; recur if not met
      :else (recur pred? (rest a-seq) (conj acc (first a-seq))))))
;;
;; loop macro version
(defn my-take-while [pred? a-seq]
  (loop [asq a-seq
         acc []]
    (cond
     ;; two stop conditions
     (empty? a-seq)             acc
     (not (pred? (first asq)))  acc
     :else (recur (rest asq)    (conj acc (first asq))))))
;;
;; (my-take-while odd? [1 2 3 4])  ;=> (1)
;; (my-take-while odd? [1 3 4 5])  ;=> (1 3)
;; (my-take-while even? [1 3 4 5]) ;=> ()
;; (my-take-while odd? [])         ;=> ()



;; Exercise 11
;; Write the function (my-drop-while pred? a-seq) that drops elements from a-seq until pred? returns false.
;;
(defn my-drop-while [pred? a-seq]
  (cond
   ;; two stop conditions
   (empty? a-seq)               '()
   (not (pred? (first a-seq)))  a-seq
   ;; No need for an accmulator, thus tail call optimized
   :else                        (recur pred? (rest a-seq))))
;;
;; (my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
;; (my-drop-while odd? [1 3 4 5])  ;=> (4 5)
;; (my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
;; (my-drop-while odd? [])         ;=> ()



;; Exercise 12
;; Write the function (seq= seq-1 seq-2) that compares two sequences for equality.
;;
;; (coll coll) -> bool
;; Check equality elementwise
(defn seq= [a-seq b-seq]
  (cond
   ;; Three stop conditions
   ;; Both are
   (and (empty? a-seq) (empty? b-seq))  true
   ;; effectively xor after and
   (or  (empty? a-seq) (empty? b-seq))  false
   ;; if the first elements are not equal, stop and return false
   (not (= (first a-seq) (first b-seq))) false
   ;; otherwise move on
   :else (recur (rest a-seq) (rest b-seq))))
;;
;; (seq= [1 2 4] '(1 2 4))  ;=> true
;; (seq= [1 2 3] [1 2 3 4]) ;=> false
;; (seq= [1 3 5] [])        ;=> false


;; Exercise 13
;; Write the function (my-map f seq-1 seq-2) that returns a sequence of the following kind . The first item is the return value of f called with the first values of seq-1 and seq-2. The second item is the return value of f called with the second values of seq-1 and seq-2 and so forth until seq-1 or seq-2 ends.
;; This is actually exactly how map works when given two sequences, but for the sake of practice don’t use map when defining my-map.
;;
(defn my-map [f seq-1 seq-2]
  (loop [s1 seq-1
         s2 seq-2
         acc []]
    (if (or (empty? s1) (empty? s2))
      ;; return acc if either is empty
      acc
      ;; otherwise recur
      (recur (rest s1) (rest s2) (conj acc (f (first s1) (first s2)))))))
;;
;; (my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)
;; (my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
;; (my-map + [1 2 3] [])        ;=> ()



;; Exercise 14
;; Write the function (power n k) that computes the mathematical expression nk.
;;
;; (number number) -> number
;; n^k
;;
;; naive version (not tail-call optimized)
(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))
;;
;; loop macro version
(defn power [n k]
  (loop [i   1
         acc 1]
    ;; loop until i is greater than k
    (if (> i k)
      acc
      (recur (inc i) (* acc n)))))
;;
;; (power 2 2)  ;=> 4
;; (power 5 3)  ;=> 125
;; (power 7 0)  ;=> 1
;; (power 0 10) ;=> 0


;; Exercise 15
;; Compute the nth Fibonacci number. The nth Fibonacci number, Fn, is defined as:
;; F0=0
;; F1=1
;; Fn=Fn−1+Fn−2
;; Write the function (fib n) which returns Fn.
;;
;; number -> number
;; calculate the fibonacci number
;;
;; naive
(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   (= n 2) 1
   (> n 2) (+ (fib (- n 1)) (fib (- n 2)))))
;;
;; Using an infinite sequence by iterate
;; Create moving pairs [0 1] [1 1] [1 2] [2 3] ... [(second prev) (sum prev)]
(defn fib1 [n]
  (nth (map first (iterate (fn [sq] [(second sq), (apply +' sq)])
                           [0 1]))
       n))
;; More readable with threading macro
(defn fib1 [n]
  (->> [0 1]                                         ; |
       ;; Infinite iteration stated above              V value passes here
       (iterate (fn [sq] [(second sq) (apply +' sq)]),  )
       ;; Get first values
       (map first                                    ,  )
       ;; Get nth value (realization occurs here)
       (#(nth % n)                                   ,  )))

;;
(fib 0) ;=> 0
(fib 1) ;=> 1
(fib 2) ;=> 1
(fib 3) ;=> 2
(fib 4) ;=> 3
(fib 5) ;=> 5
(fib 6) ;=> 8
(fib 10) ;=> 55

;; Exercise 16
;; Write the function (my-repeat how-many-times what-to-repeat) that generates a list with what-to-repeat repeated how-man
;; y-times number of times.
;;
;; number, any -> coll
;;
;; naive
(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))
;;
;; multi-arity version
(defn my-repeat
  ;; body 1
  ([n what] (my-repeat n what '()))
  ;; body 2 with accumulator
  ([n what acc]
     (if (<= n 0)
       acc
       (recur (dec n) what (cons what acc)))))
;;
;; loop macro version
(defn my-repeat [how-many what]
  (loop [n how-many
         acc []]
    (if (<= n 0)
      acc
      (recur (dec n) (conj acc what)))))
;;
;; (my-repeat 2 :a)    ;=> (:a :a)
;; (my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
;; (my-repeat -1 :a)   ;=> ()


;; Exercise 17
;; Write the function (my-range up-to) that works like this:
;;
;; naive
(defn my-range [up-to]
  (let [dec-up-to (dec up-to)]
    (if (<= up-to 0)
      '()
      (cons dec-up-to (my-range dec-up-to)))))
;;
;; Loop version by incrementing
(defn my-range [up-to]
  (loop [i    0
         acc '()]
    (if (= i up-to)
      acc
      (recur (inc i) (conj acc i)))))
;;
(my-range 0)  ;=> ()
(my-range 1)  ;=> (0)
(my-range 2)  ;=> (1 0)
(my-range 3)  ;=> (2 1 0)


;; Exercise 18
;; Write the functions tails and inits that return all the suffixes and prefixes of a sequence, respectively.
;; Hint: You can use reverse and map.
;; (reverse [1 2 3]) ;=> (3 2 1)
;; (reverse [2 3 1]) ;=> (1 3 2)
;;
;; naive
(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (cons a-seq (tails (rest a-seq)))))
;;
;; Using infinite sequence
(defn tails [a-seq]
  (concat (take-while (complement empty?) (iterate rest a-seq))
          ;; need to add an empty one at the end
          '(())))
;;
(tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())
;;
;;
(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))
;;
(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))
; You can output the tails and inits in any order you like.
(inits [1 2 3 4]) ;=> ((1 2) () (1 2 3) (1) (1 2 3 4))



;; Exercise 19
;; Write the function (rotations a-seq) that, when given a sequence, returns all the rotations of that sequence.
;;
;; iterate version
(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (take (count a-seq) (iterate (fn [x] (conj (vec (rest x)) (first x))) a-seq))))
;;
;; loop version. not complete
;; (defn rotations [a-seq]
;;   (loop [i   0
;;          acc '()]
;;     (if (= i (count a-seq))
;;       acc
;;       (recur (inc i) (conj acc (concat (drop i a-seq) (take i a-seq)))))))
;;
(rotations [])        ;=> (())
(rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
(rotations [:a :b])   ;=> ((:a :b) (:b :a))
; The order of rotations does not matter.
(rotations [:a :b])   ;=> ((:b :a) (:a :b))
(rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
(count (rotations [6 5 8 9 2])) ;=> 5
;; Keep in mind the function concat.
(concat [1 2 3] [:a :b :c]) ;=> (1 2 3 :a :b :c)
(concat [1 2] [3 4 5 6])    ;=> (1 2 3 4 5 6)



;; Exercise 20
;; Write the function (my-frequencies a-seq) that computes a map of how many times each element occurs in a sequence. E.g.
(defn my-frequencies-helper [freqs a-seq]
  [:-])
;; :
;; You’ll want to structure your code like this:
(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (recur
     (merge-with + freqs (zipmap [(first a-seq)] [1]))
     (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))
;; Where my-frequencies-helper is a recursive helper function.
;; (my-frequencies []) ;=> {}
;; (my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}


;; Exercise 21
;; Write the function (un-frequencies a-map) which takes a map produced by my-frequencies and generates a sequence with the corresponding numbers of different elements.
;; The order of elements in the output sequence doesn’t matter.
;; Hint 1: Remember that you can use first and rest on a map too!
;; (first {:a 1 :b 2}) ;=> [:a 1]
;; (rest {:a 1 :b 2 :c 3}) ;=> ([:b 2] [:c 3])
;; Hint 2: There are multiple ways to implement this, but consider using concat and repeat.
(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [map-first-elt (first a-map)]
        (concat (repeat (second map-first-elt) (first map-first-elt)) (un-frequencies (rest a-map))))))
;;
;; (un-frequencies {:a 3 :b 2 "^_^" 1})             ;=> (:a :a :a "^_^" :b :b)
;; (un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
;; (my-frequencies (un-frequencies {:a 100 :b 10})) ;=> {:a 100 :b 10}



;; Exercise 22
;; Implement (my-take n coll) that returns n first items of coll.
(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ;; stop if there is no more to take in n or coll
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))
;;
;; (my-take 2 [1 2 3 4]) ;=> (1 2)
;; (my-take 4 [:a :b])   ;=> (:a :b)



;; Exercise 23
;; Implement (my-drop n coll) that returns all but the n first items of coll.
(defn my-drop [n coll]
  (if (zero? n)
    coll
    (recur (dec n) (rest coll))))
;;
;; (my-drop 2 [1 2 3 4]) ;=> (3 4)
;; (my-drop 4 [:a :b])   ;=> ()


;; Exercise 24
;; Implement the function (halve a-seq) that takes a sequence and returns one vector with two elements. The first element is the first half of a-seq and the second element is the second half of a-seq.
;; To turn a result of division into an integer use int.
;;
(defn halve [a-seq]
  (let [n (quot (count a-seq) 2)]
    [(take n a-seq) (drop n a-seq)]))
;;
;; (int (/ 7 2)) ;=> 3
;; (halve [1 2 3 4])   ;=> [(1 2) (3 4)]
;; (halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
;; (halve [1])         ;=> [() (1)]



;; Exercise 25
;; Write the function (seq-merge a-seq b-seq) that takes two (low to high) sorted number sequences and combines them into one sorted sequence. Don’t use sort (nor implement it yourself, yet).
;;
;; two seq -> one seq
;; loop over the second seq, insert values from the first seq
;; naive version (tail call optimized)
(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (recur (rest a-seq) (concat
                         (filter #(< % (first a-seq)) b-seq)
                         (list (first a-seq))
                         (filter #(>= % (first a-seq)) b-seq)))))
;;
;; loop version (not more efficient)
;; (defn seq-merge [a-seq b-seq]
;;   (loop [as a-seq
;;          bs b-seq]
;;     ;;
;;     (if (empty? as)
;;       ;; if as is empty, return the merged list
;;       bs
;;       ;; otherwise recur
;;       (recur (rest as) (concat
;;                         (filter #(< % (first as)) bs)
;;                         (list (first as))
;;                         (filter #(>= % (first as)) bs))))))
;;
(seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
(seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)


;; Exercise 26
;; Write the function (merge-sort a-seq) that implements merge sort.
;; The idea of merge sort is to divide the input into subsequences using halve, sort the subsequences recursively and use the seq-merge function to merge the sorted subsequences back together.
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
;;
;; seq -> seq
;; sort numbers
;; algorithm: divid into two
(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))
;;
;; (merge-sort [])                 ;=> ()
;; (merge-sort [1 2 3])            ;=> (1 2 3)
;; (merge-sort [5 3 4 17 2 100 1]) ;=> (1 2 3 4 5 17 100)




;; Exercise 27
;; 2 points
;; Write the function split-into-monotonics that takes a sequence and returns the sequence split into monotonic pieces. Examples:
;;
;; seq -> seq of seqs
;; divide a seq into monotonic sequences
(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))
;;
;; naive version using monotonic?
(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()

    ;; assign the longest monotonic seq to seq-mono
    (let [seq-mono (last (filter #(monotonic? %) (rest (inits a-seq))))]
      ;; combine
      (cons seq-mono
            (split-into-monotonics (drop (count seq-mono) a-seq))))))
;;
;; (split-into-monotonics [0 1 2 1 0])   ;=> ((0 1 2) (1 0))
;; (split-into-monotonics [0 5 4 7 1 3]) ;=> ((0 5) (4 7) (1 3))
;; ;; Hint: You might find useful the functions take-while, drop and inits. Make sure that your inits returns the prefixes from the shortest to the longest.
;; (inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))
;; (rest (inits [1 2 3 4]))


;; (filter #(monotonic? %) (rest (inits [0 5 4 7 1 3])))
;; (last (filter #(monotonic? %) (rest (inits [0 5 4 7 1 3]))))



;; Exercise 28
;; 3 points
;; Given a sequence, return all permutations of that sequence.
;;
;; set -> seq of seqs
;; Create all possible permutation of values in a set
;;
;; seq -> seq
;; swap first and ith where i = 0,..,n
(defn swap-1st-and-ith [coll i]
  (if (zero? i)
    ;; No swapping if i = 0
    coll
    ;; Otherwise swap first and ith positions
    (flatten [(nth coll i) (rest (take i coll)) (first coll) (nthrest coll (inc i))])))
;;
(swap-1st-and-ith (range 0 10) 0)
(swap-1st-and-ith (range 0 10) 1)
(swap-1st-and-ith (range 0 10) 2)
(swap-1st-and-ith (range 0 10) 3)
(swap-1st-and-ith (range 0 10) 4)
(swap-1st-and-ith (range 0 10) 5)
;;
;; first level only
(defn tree-of-perm [a-seq]
  (let [;; Assing length
        len  (count a-seq)
        ;; Create all possible first-level swapped sequences
        seqs (map #(swap-1st-and-ith a-seq %) (range 0 len))]
    seqs))
(tree-of-perm (into [] #{1 5 3 7}))
;;
;; Recursive version
(defn tree-of-perm [a-seq]
  (let [;; Assing length
        len  (count a-seq)]

    (if (<= len 1)
      ;; base case: No swapping if only one element remaining
      a-seq
      ;; otherwise: Swapping, and then map recursions
      (map #(cons (first %) (tree-of-perm (rest %)))
           ;; Create swapped sequences
           (map #(swap-1st-and-ith a-seq %) (range 0 len))))))
;;
(tree-of-perm (into [] #{1 5 3 7}))
;;
;;
;; merge helper function bottom up
(defn merge-up [a-tree]
  (if (= 3 (count a-tree))
    ;; If 3 elements, at terminal part. Create two seqs
    [(conj (second a-tree) (first a-tree))
     (conj (last a-tree) (first a-tree))]

    ;; If not go down by mapping
    (map (fn [x] (map #(conj % (first a-tree)) x))
         (map merge-up (rest a-tree)))))

(merge-up '(3 (5 7) (7 5)))
(merge-up '(1 (3 (5 7)
                 (7 5))
              (5 (3 7)
                 (7 3))
              (7 (5 3)
                 (3 5))))

(map merge-up '((1 (3 (5 7)
                      (7 5))
                   (5 (3 7)
                      (7 3))
                   (7 (5 3)
                      (3 5)))
                ;;
                (3 (1 (5 7) (7 5)) (5 (1 7) (7 1)) (7 (5 1) (1 5)))
                (5 (3 (1 7) (7 1)) (1 (3 7) (7 3)) (7 (1 3) (3 1)))
                (7 (3 (5 1) (1 5)) (5 (3 1) (1 3)) (1 (5 3) (3 5)))))

(map merge-up (tree-of-perm (into [] #{1 5 3 7})))
(partition-all 4 (flatten (map merge-up (tree-of-perm (into [] #{1 5 3 7})))))
;
;;
;; Finally permutation
(defn permutations [a-set]
  (let [;; Convert to a vector
        a-seq (into [] a-set)
        ;; Assing length
        len  (count a-seq)]
    ;;
    (if (zero? len)
      ;; if zero, return empty permutation
      '(())
      ;; Otherwise, create permutations
      (partition-all len (flatten (map merge-up (tree-of-perm a-seq)))))))

;;
(permutations #{})
;=> (())
(permutations #{1 5 3})
;=> ((1 5 3) (5 1 3) (5 3 1) (1 3 5) (3 1 5) (3 5 1))
;; The order of the permutations doesn’t matter.
(permutations #{1 5 3 7})




;; Write a C program to print all permutations of a given string
;; http://www.geeksforgeeks.org/write-a-c-program-to-print-all-permutations-of-a-given-string/



;; Exercise 29
;; 3 points
;; Given a set, return the powerset of that set.
;;
(defn powerset [a-set]
  [:-])
;;
(powerset #{})      ;=> #{#{}}
(powerset #{1 2 4}) ;=> #{#{} #{4} #{2} #{2 4} #{1} #{1 4} #{1 2} #{1 2 4}}

