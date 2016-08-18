(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (nth coll 0)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (nth a-seq 0)
      (max-element (conj
                     (rest (rest a-seq))
                     (max (first a-seq)(second a-seq)))))))

(defn seq-max [seq-1 seq-2]
  (cond
    (> (count seq-1)(count seq-2)) seq-1
    :else seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (nth a-seq 0)
      (longest-sequence (conj
                          (rest (rest a-seq))
                          (seq-max (first a-seq)(second a-seq)))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (cond
      (pred? (first a-seq)) (cons (first a-seq)
                                  (my-filter pred? (rest a-seq)))
      :else (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    ((complement pred?) (first a-seq)) ()
    :else (cons (first a-seq)
                (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    ((complement pred?) (first a-seq)) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or ((complement boolean) (first a-seq))
        ((complement boolean) (first b-seq))) false
    ((complement =) (first a-seq)(first b-seq)) false
    :else (seq= (rest a-seq)(rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1)(empty? seq-2)) seq-2
    :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (if (= 1 k)
      n
      (* n (power n (dec k))))))

(defn fib [n]
  (if (zero? n)
    0
    (if (< n 3)
      1
      (+ (fib (- n 1))
         (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (neg? how-many-times) ()
    (zero? how-many-times) nil
    :else (cons what-to-repeat
            (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to)
      (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () a-seq)
    (cons a-seq
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons () a-seq)
    (cons a-seq
      (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
    (if (empty? a-seq)
    (quote (()))
      (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
     freqs
     (let [newfreqs (if (contains? freqs (first a-seq))
                      (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
                      (assoc freqs (first a-seq) 1))]
       (my-frequencies-helper newfreqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-map]
  (if (empty? a-map)
    nil
    (cons (repeat (second (first a-map))
                  (first (first a-map)))
          (un-frequencies-helper (rest a-map)))))

(defn un-frequencies [a-map]
  (apply concat (un-frequencies-helper a-map)))

(defn my-take [n coll]
  (if ((complement boolean) (first coll))
    nil
    (if (zero? n)
      nil
      (cons (first coll)
        (my-take (dec n)
                 (rest coll))))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (cons (take (quot (count a-seq) 2) a-seq)
        (cons (drop (quot (count a-seq) 2) a-seq)
              {})))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (if (empty? b-seq)
      a-seq
      (cond
        (> (first a-seq)
           (first b-seq))
        (cons (first b-seq)
              (seq-merge a-seq (rest b-seq)))
        :else (cons (first a-seq)
                    (seq-merge (rest a-seq) b-seq))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[left right] (halve a-seq)]
      (seq-merge (merge-sort left)
                 (merge-sort right)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

;;;;;;;;;;;;;;;;;;;;;;;


(defn concat-every [coll]
  (if (empty? coll)
    nil
    (cons (cons (first (first coll))
                (first (rest (first coll))))
          (concat-every (rest coll)))))

(concat-every [[4 [1 1]] [4 [2 2]]])

(defn add-value-every [val coll]
    (concat-every (map vector (repeat (count coll) val) coll)))

(add-value-every 1 [[1 2 3] [222]])

(defn powerset-helper [a-set b-set]
  (if (empty? b-set)
    a-set
    (powerset-helper (add-value-every (first b-set) (vector a-set)) (rest b-set))))

(defn powerset [a-set]
  (powerset-helper a-set a-set))

;          (repeat 2 (cons (cons (first a-set)
;                      (rest a-set))
;                (powerset (rest a-set))))))


;  (reduce (fn [a x]
;            (->> a
;                 (map #(set (concat #{x} %)))
;                 (concat a)
;                 set))
;          #{#{}} a-set))

(powerset #{})      ;=> #{#{}}
(powerset #{1 2})
                  ;=> #{#{} #{4} #{2} #{2 4} #{1} #{1 4} #{1 2} #{1 2 4}}

;(count (powerset (range 10))) ;=> 1024)










