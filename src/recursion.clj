(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

; (product [1 2 4])
; (* 1 (product [2 4]))
; (* 1 (* 2 (product [4])))
; (* 1 (* 2 (* 4 (product []))))
; (* 1 (* 2 (* 4 1)))
; (* 1 (* 2 4))
; (* 1 8)
; 8

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (= (count coll) 1))
    (first coll)
    (my-last (rest coll))))

; Doesn't work with a sequence of negative values.
;(defn max-element [a-seq]
;  (if (empty? a-seq)
;    nil
;    (max (first a-seq) (or (max-element (rest a-seq)) 0))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (let [elem (max-element (rest a-seq))]
      (if (nil? elem)
        (first a-seq)
        (max (first a-seq) elem)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (let [b-seq (longest-sequence (rest a-seq))]
      (if (nil? b-seq)
        (first a-seq)
        (seq-max (first a-seq) b-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
          (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)         false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
   (cons (f (first seq-1) (first seq-2))
         (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
   :else (+ (fib (- n 2)) (fib (dec n)))))

(defn my-repeat [n what]
  (if (<= n 0) '()
    (cons what (my-repeat (dec n) what))))

(defn my-range [up-to]
  (if (zero? up-to) '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
   (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(())
  (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (my-frequencies-helper (if (contains? freqs (first a-seq))
                             (assoc freqs (first a-seq)
                               (inc (get freqs (first a-seq))))
                             (assoc freqs (first a-seq) 1))
                           (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [x] (repeat (get a-map x) x)) (keys a-map))))

(defn my-take [n coll]
  (seq (subvec coll 0 (min (count coll) n))))

(defn my-drop [n coll]
  (subvec coll (min (count coll) n)))

(defn halve [a-seq]
  [(subvec a-seq 0 (int (/ (count a-seq) 2))) (subvec a-seq (int (/ (count a-seq) 2)))])

(defn seq-merge-helper [new-seq a-seq b-seq]
  (cond
   (empty? a-seq) (concat new-seq b-seq)
   (empty? b-seq) (concat new-seq a-seq)
   :else (if (<= (first a-seq) (first b-seq))
           (seq-merge-helper (conj new-seq (first a-seq)) (rest a-seq) b-seq)
           (seq-merge-helper (conj new-seq (first b-seq)) a-seq (rest b-seq)))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper [] a-seq b-seq))

(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq)) a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn monotonics-helper [tonics n a-seq]
  (if (empty? a-seq) (reverse tonics)
    (if (and (< n (count a-seq)) (monotonic? (take (inc n) a-seq)))
      (monotonics-helper tonics (inc n) a-seq)
      (monotonics-helper (conj tonics (take n a-seq)) 0 (drop n a-seq)))))

(defn split-into-monotonics [a-seq]
  (monotonics-helper '() 0 a-seq))

(defn permutations-helper [start a-set]
  (if (empty? a-set) start
    (flatten (for [number a-set]
      (let [others (remove (fn [x] (= x number)) a-set)]
        (permutations-helper (conj start number) others))))))

(defn permutations [a-set]
  (if (empty? a-set) '(())
    (partition (count a-set) (permutations-helper '() a-set))))
    ; Nice flatten -> partition hack to get rid of inner sequences, not proud of it, but it works. :D

(defn subset-helper [a-set current-set target]
  (if (= (count current-set) target) [current-set]
    (let [remaining (clojure.set/difference a-set current-set)]
      (for [elem remaining
            solution (subset-helper a-set (conj current-set elem) target)]
        solution))))

(defn subsets [a-set size]
  (set (subset-helper a-set #{} size)))

(defn powerset [a-set]
  (set (apply concat (for [i (range 0 (inc (count a-set)))]
    (subsets (set a-set) i)))))

;(powerset #{})      ;=> #{#{}}
;(powerset #{1 2 4}) ;=> #{#{} #{4} #{2} #{2 4} #{1} #{1 4} #{1 2} #{1 2 4}}
;(count (powerset (range 10))) ;=> 1024








