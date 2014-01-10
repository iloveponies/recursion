(ns recursion)

(defn product [xs]
  (if (empty? xs)
    1
    (* (first xs) (product (rest xs)))))

(defn singleton? [xs]
  (if (empty? xs)
    false
    (empty? (rest xs))))

(defn my-last [xs]
  (cond
   (empty? xs) nil
   (singleton? xs) (first xs)
   :else (my-last (rest xs))))

(defn max-element [xs]
  (cond
   (empty? xs) nil
   (singleton? xs) (first xs)
   :else (max (first xs) (max-element (rest xs)))))

(defn seq-max [xs ys]
  (let [c1 (count xs)
        c2 (count ys)]
    (if (< 0 (- c1 c2))
      xs
      ys)))

(defn longest-sequence [seqs]
  (cond
    (empty? seqs) nil
    (singleton? seqs) (first seqs)
    :else (seq-max (first seqs) (longest-sequence (rest seqs)))))

(defn my-filter [pred? xs]
  (if (empty? xs)
    xs
    (if (pred? (first xs))
      (cons (first xs) (my-filter pred? (rest xs)))
      (my-filter pred? (rest xs)))))

(defn sequence-contains? [x xs]
  (cond
    (empty? xs) false
    (== x (first xs)) true
    :else (sequence-contains? x (rest xs))))

(defn my-take-while [pred? xs]
  (cond
   (empty? xs) xs
   (pred? (first xs)) (cons (first xs) (my-take-while pred? (rest xs)))
   :else ()))

(defn my-drop-while [pred? xs]
  (cond
   (empty? xs) xs
   (pred? (first xs)) (my-drop-while pred? (rest xs))
   :else xs))

(defn seq= [xs ys]
  (if (or (empty? xs) (empty? ys))
    (and (empty? xs) (empty? ys))
    (and (== (first xs) (first ys)) (seq= (rest xs) (rest ys)))))

(defn my-map [f xs ys]
  (if (or (empty? xs) (empty? ys))
    ()
    (cons (f (first xs) (first ys)) (my-map f (rest xs) (rest ys)))))

(defn power [x k]
  (cond
    (zero? k) 1
    (= 1 k ) x
    :else (* x (power x (dec k)))))

(defn fib [x]
  (cond
     (== x 0 ) 0
     (== x 1 ) 1
     :else (+ (fib (- x 1)) (fib (- x 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
   ()
   (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [xs]
  (if (empty? xs)
    '(())
    (conj (tails (rest xs)) (seq xs))))


(defn inits [xs]
   (reverse (map reverse (tails (reverse xs)))))

(defn rotations-helper [x xs pos]
  (if (>= pos (count xs)) 
    (seq [(cons x xs)])
    (concat [(concat (drop pos xs) [x] (take pos xs))] (rotations-helper x xs (+ pos 1)))))

(defn rotations [xs]
  (if (empty? xs) 
    '(())
    (rotations-helper (first xs) (rest xs) 0)))

(defn my-frequencies-helper [xs counts]
  (if (empty? xs)
    counts
    (let [
          elem (first xs)
          the-rest (rest xs)
          elem-count (get counts elem)
          new-elem-count (if elem-count (inc elem-count) 1)
          ]
        (my-frequencies-helper the-rest (assoc counts elem new-elem-count)))))
   
(defn my-frequencies [xs]
    (my-frequencies-helper xs {}))
  
(defn un-frequencies [xs]
  (if (empty? xs)
    ()
    (let [
          repeat-params (seq (reverse (first xs)))
          repeats (apply repeat repeat-params)
          ]
      (concat repeats (un-frequencies (rest xs))))))


(defn my-take [n xs]
  (if (or (empty? xs) (<= n 0)) 
    ()
    (cons (first xs) (my-take (dec n) (rest xs)))))

(defn my-drop [n xs]
  (cond
   (or (empty? xs) (<= (count xs) n)) ()
   (<= n 0) xs
   :else (my-drop (dec n) (rest xs))
    ))

(defn halve [xs]
  (if (empty? xs)
    ()
    (let [half-size (int (/ (count xs) 2))]
      [(take half-size xs) (drop half-size xs)])))


(defn seq-merge [xs ys]
  (cond
   (empty? xs) ys
   (empty? ys) xs
   :else (let [ cond? (fn [x] (< x (first ys)))
               take-until-first-y (fn [zs] (take-while cond? zs))
               drop-until-first-y (fn [zs] (drop-while cond? zs))
               ]
           (concat (take-until-first-y xs) [(first ys)] (seq-merge (drop-until-first-y xs) (rest ys))))))

(defn merge-sort [xs]
  (cond
   (empty? xs) ()
   (== (count xs) 1) xs
   :else (let [params (map merge-sort (halve xs))] 
           (apply seq-merge params))))

(defn split-into-monotonics [xs]
  (if (empty? xs)
    ()
    (let [
          prefixes (inits xs)
          longest-monotonic-prefix (last (take-while (fn [ys] (or (empty? ys) (apply > ys) (apply < ys))) prefixes))
          ]
      (concat [longest-monotonic-prefix] (split-into-monotonics (drop (count longest-monotonic-prefix) xs))))))


(defn insert-first [x seqs]
  (if (empty? seqs)
    '(())
    (map (fn [xs] (cons x xs)) seqs)))

(defn permutations [xs]
  (let [
        permutations-helper (fn [ys]
                              (cond
                               (empty? ys) ()
                               (<= (count ys) 2) (seq [ys])
                               :else (let [
                                           rest-ys-perms (permutations (rest ys))
                                           first-y (first ys)
                                           ] (insert-first first-y rest-ys-perms))))
        ]
    (if (empty? xs)
      '(())
      (apply concat (map permutations-helper (rotations xs))))))

(defn inits-to-set [a-set xs]
  (if (empty? xs)
    a-set
    (set (concat a-set (set (map set (inits xs)))))))

;; (defn powerset [xs]
;;   (do (println xs)
;;       (if (empty? xs)
;;         #{#{}}
;;         (reduce inits-to-set #{} (permutations xs)))))

;(powerset [0]); #{#{} #{0}}
;(powerset [0 1]);#{#{} #{0} #{1} #{0 1}}

;; (let [xs [0 1 2 3 4 5 6 7]]
;;   (== (power 2 (count xs)) (count (powerset xs))));#{#{} #{0} #{1} #{2} #{0 1} #{0 2} #{1 2} #{0 1 2}}


(defn my-fun [xs]
  (if (empty? xs)
    #{#{}}
    (set (map set (concat [[]  [(first xs)] xs] ( let [insert-first-x (fn [seqs] (insert-first (first xs) seqs) )] 
                                                  (apply concat (map insert-first-x (map my-fun (rotations (rest xs)))))))))))

;(let [xs [0 1 2]]  (set (map set (concat #{[(first xs)] xs}
;(insert-first 0 #{#{ 3 4} #{5 6}})))))


;(powerset [0 1 2 3 4 5 6])
(defn powerset [xs] (set (apply concat (map my-fun (rotations xs)))))

(powerset [])


;(set (map set (insert-first 0 #{#{ 0 1 2} #{0 3 4} #{0 1 2})))



;(map set (insert-first 2 (set (map set ( rotations [0 1])))))



;; 0 1 2 3   0, 0 1, 0 1 2, 0 1 2 3
;;   1 3 2
;;     2 3
;;       3
;;   2 3 1   0 2, 0 2 3
;;     1 3
;;       3
;;   3 1 2   0 3, 0 3 1
;;     2 1
;;       1




;; 1 2 3 0
;;   3 0 2
;;   0 2 3


;; 2 3 0 1
;;   0 1 3
;;   1 3 0

;; 3 0 1 2
;;   1 2 0
;;   2 0 1



