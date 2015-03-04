(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))


(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (if (empty? coll)
      nil
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (singleton? a-seq)
    (first a-seq)
    (if (empty? a-seq)
      nil
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (singleton? a-seq)
    (first a-seq)
    (if (empty? a-seq)
      nil
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (empty? a-seq) false
    (empty? b-seq) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (and (empty? seq-1) (empty? seq-2)) seq-1
    (empty? seq-1) '()
    (empty? seq-2) '()
    :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
     (zero? n) 0
     (== 1 n) 1
     :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [n what-to-repeat]
  (cond
     (<= n 0) '()
     (== 1 n) (cons what-to-repeat `())
     :else (cons what-to-repeat (my-repeat (- n 1) what-to-repeat))))

(defn my-range [up-to]
  (cond
     (zero? up-to) '()
     :else (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (sort #(compare (count %1) (count %2)) (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [rotate (fn [r-set] (concat (rest r-set) [(first r-set)]))
        helper (fn helper [b-seq]
                 (let [rotated (rotate b-seq)]
                   (if (= a-seq rotated)
                     [a-seq]
                     (cons rotated (helper rotated)))))]
    (if (empty? a-seq)
      '(())
      (helper a-seq))))


(defn my-frequencies-helper [freqs a-seq]
  (cond
     (empty? a-seq) freqs
     (contains? freqs (first a-seq)) (my-frequencies-helper freqs (rest a-seq))
     :else (my-frequencies-helper (assoc freqs (first a-seq)
                                    (count (filter (fn [x] (= x (first a-seq))) a-seq)))
                                  (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
    (if (empty? a-map)
      '()
      (concat (repeat (val (first a-map)) (key (first a-map)))
              (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond
     (empty? coll) coll
     (= n 0) '()
     :else (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond
     (empty? coll) coll
     (= n 0) coll
     :else (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
     :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))


(defn merge-sort [a-seq]
  (cond
     (empty? a-seq) a-seq
     (= 1 (count a-seq)) a-seq
     :else (let [[first-half second-half] (halve a-seq)]
             (seq-merge (merge-sort first-half)
                        (merge-sort second-half)))))

(defn split-into-monotonics [a-seq]
  (let [initials (inits a-seq)
        ismono? (fn ismono? [mono-seq]
                  (cond
                     (empty? mono-seq) true
                     (apply <= mono-seq) true
                     :else (apply >= mono-seq)))
        mono (last (filter (fn [x] (ismono? x)) initials))]
    (cond
       (empty? a-seq) '()
       :else (cons mono
                   (split-into-monotonics (drop (count mono) a-seq))))))


 (defn permutations [a-set]
  (if (or (empty? a-set) (singleton? a-set))
    (list a-set)
    (mapcat
     #(map (fn [l] (cons (first %) l)) (permutations (rest %)))
     (rotations a-set))))

 (defn powerset [a-set]
  (if (empty? a-set) (list '())
    (let [ps (powerset (rest a-set))]
      (clojure.set/union ps
        (map #(cons (first a-set) %) ps)))))

