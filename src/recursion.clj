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
  
  (and ; BOTH
   ;; first element is not nil
   ((complement nil?) (first coll))
   ;; rest collection is empty
   (empty? (rest coll))))
;;
;; (singleton? [1])     ;=> true
;; (singleton? #{2})    ;=> true
;; (singleton? [])      ;=> false
;; (singleton? [1 2 3]) ;=> false



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
;; 
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


(defn my-repeat [how-many-times what-to-repeat]
  [:-])

(defn my-range [up-to]
  [:-])

(defn tails [a-seq]
  [:-])

(defn inits [a-seq]
  [:-])

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

