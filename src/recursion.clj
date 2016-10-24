(ns recursion)


(defn product [coll]
  (cond
   (empty? coll) 1
   :else (* (first coll)
      (product (rest coll)))))

;(defn singleton? [coll]
;  (cond
;   (empty? coll) false
;   (== (count coll) 1) true
;   :else false))

(defn singleton? [coll]
  (cond
   (empty? coll) false
   (empty? (rest coll)) true
   :else false))



(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))


(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   (< (first a-seq) (first (rest a-seq))) (max-element (rest a-seq))
   :else (max-element (cons (first a-seq) (rest (rest a-seq))))))


;(defn max-element [a-seq]
;  (cond
;   (empty? a-seq) nil
;   (singleton? a-seq) (first a-seq)
;   (= (count a-seq) 2) (first a-seq)
;   :else (cons (max (first a-seq) (first (rest a-seq)))
;               (max-element (rest (rest a-seq))))))
;                       (rest (rest a-seq))))))



;(defn seq-max [seq-1 seq-2]
;    (max (count seq-1) (count seq-2)))

  (defn seq-max [seq-1 seq-2]
    (cond
     (> (count seq-1) (count seq-2)) seq-1
     :else seq-2))


;(defn longest-sequence [a-seq]
;  (cond
;   (empty? a-seq) nil
;   (singleton? a-seq) (first a-seq)
;   :else (cons (seq-max (first a-seq) (first (rest a-seq)))
;               (longest-sequence (rest (rest a-seq))))))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (longest-sequence (cons
                            (seq-max (first a-seq) (second a-seq))
                            (rest (rest a-seq))))))



(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq))
   (cons (first a-seq)
         (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (not= elem (first a-seq)) (sequence-contains? elem (rest a-seq))
   :else true))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq)
                               (my-take-while pred? (rest a-seq)))
   :else '()))


;Write the function (my-drop-while pred? a-seq) that drops elements
;from a-seq until pred? returns false.


(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else (into '() (reverse a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq)
        (empty? b-seq)) true
   (or (empty? a-seq)
       (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1)
       (empty? seq-2)) '()
   :else (cons (f (first seq-1)
                  (first seq-2))
               (my-map f (rest seq-1)
                       (rest seq-2)))))

(defn power [n k]
  (cond
   (= k 0) 1
   (= k 1) n
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0) '()
   :else (cons what-to-repeat
               (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (= up-to 0) '()
   :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (let [my-seq (seq a-seq)]
    (cond
     (empty? a-seq) '(())
     :else  (cons my-seq
                  (tails (rest a-seq))))))

;; (defn inits [a-seq]
;;    (let [rev-seq (reverse a-seq)]
;;   (cond
;;    (empty? a-seq) '()
;;    :else  (cons rev-seq
;;                (tails (rest rev-seq))))))


(defn inits [a-seq]
  (let [rev-seq (reverse a-seq)]
    (cond
     (empty? a-seq) '(())
     :else  (cons (seq a-seq)
                  (inits (drop-last a-seq))))))



(defn rotation-helper [n a-seq]
  (cond
   (= 0 n) '()
   :else (cons
          (concat (drop n a-seq) (take n a-seq))
          (rotation-helper (dec n) a-seq))))



(defn rotations [a-seq]
  (let [n (count a-seq)]
    (rotation-helper n a-seq)))

;;Problem
;; (rotations []) => (()) however ()


(defn my-frequencies-helper [freqs a-seq]
  (cond (empty? a-seq) freqs
        :else (my-frequencies-helper
               (update-in freqs [(first a-seq)] (fnil inc 0))
               (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [what-to-repeat (first (first a-map))
        how-many-times (second (first a-map))]
    (cond
     (empty? a-map) '()
     :else (concat (repeat how-many-times what-to-repeat)
                   (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
   (or (= n 0) (= (count coll)0)) '()
   :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (> n 0) (my-drop (dec n) (rest coll))
   :else coll))

(defn halve [a-seq]
  (let [split-point (int (/ (count a-seq) 2))]
    (split-at split-point a-seq)))

;; (defn seq-merge [a-seq b-seq]
;;   (cond
;;    (> (first a-seq) (first b-seq))
;;        (cons (first a-seq)
;;             (cons (first b-seq)
;;                    (seq-merge (rest a-seq) (rest b-seq))))
;;    :else (cons (first b-seq)
;;             (cons (first a-seq)
;;                   (seq-merge (rest a-seq) (rest b-seq))))))

(defn seq-merge [a-seq b-seq]
  (let [head-a (first a-seq)
        head-b (first b-seq)
        tail-a (rest a-seq)
        tail-b (rest b-seq)]
    (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (and (empty? a-seq) (empty? b-seq)) '()
     (<= head-a head-b) (cons head-a (seq-merge tail-a b-seq))
     :else (cons head-b (seq-merge a-seq tail-b)))))



(defn merge-sort [a-seq]
  (let [first-half (first (halve a-seq))
        second-half (second (halve a-seq))]
    (cond
     (or (= (count a-seq) 1)       (= (count a-seq) 0)) a-seq
     :else (seq-merge (merge-sort first-half) (merge-sort second-half)))))


(defn monotonic? [a-seq]
  (cond
   (or (apply <= a-seq)
       (apply >= a-seq)) true
       :else false))

(defn split-into-monotonics [a-seq]
  (let [gen-chunks (rest (reverse
                          (inits (seq a-seq))))
        get-chunks (last (take-while monotonic?
                                     gen-chunks))]
    (cond
      (empty? a-seq) '()
      :else (cons get-chunks
                  (split-into-monotonics
                   (subvec a-seq (count get-chunks)))))))

;; (defn permutations [a-set]
;;   (let [perm-helper (fn [elem elem1 elem2 n]
;;                               (cond
;;                                 (= n 0) '()
;;                                 :else (cons
;;                                        (cons elem (cons elem1 (cons elem2 '())))
;;                                        (permutations-helper elem1 elem2 elem (dec n)))))]
;;   (cond
;;    (empty? a-set) '(())
;;    :else (perm-helper (first a-set) (second a-set) (last a-set) 3))))

(defn permutations [a-set]
  (cond
    (empty? a-set) '()
    :else (cons (reverse (reverse a-set)) (cons (reverse a-set) (permutations #{})))))

;; (defn powerset [a-set]
;;   (cond
;;    (empty? a-set) a-set
;;    :else (cons a-set
;;                (set (powerset (rest a-set))))))

(defn powerset [a-set]
  (if (empty? a-set) #{#{}}
      (clojure.set/union (powerset (next a-set))
                         (map #(conj % (first a-set))
                              (powerset (next a-set))))))


