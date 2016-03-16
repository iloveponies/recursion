(ns recursion)

(defn product [coll]
  (let [prodhelper (fn [coll ret]
                    (if (empty? coll)
                      ret
                      (recur (rest coll) (* ret (first coll)))))]
    (prodhelper coll 1)))

(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (recur (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (let [helper
          (fn [a-seq ret]
            (if (empty? a-seq)
              ret
              (recur (rest a-seq) (max (first a-seq) ret))))]
      (helper (rest a-seq) (first a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [helper
        (fn [s1 s2]
          (cond
            (empty? s1) seq-2
            (empty? s2) seq-1
            :else (recur (rest s1) (rest s2))))]
    (helper seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if
    (empty? a-seq)
    a-seq
    (let [res (my-filter pred? (rest a-seq))]
      (if (pred? (first a-seq))
        (cons (first a-seq) res)
        res))))


(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
    false
    (== (first a-seq) elem)
    true
    :else
    (recur elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
    a-seq
    (pred? (first a-seq))
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
    '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
    a-seq
    (pred? (first a-seq))
    (recur pred? (rest a-seq))
    :else
    a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
    true
    (or (empty? a-seq) (empty? b-seq))
    false
    (not (== (first a-seq) (first b-seq)))
    false
    :else
    (recur (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (cond
    (== k 0)
    1
    (== (mod k 2) 0)
    (recur (* n n) (/ k 2))
    :else
    (* n (power n (dec k)))))

(defn fib [n]
  (let [helper
        (fn [f1 f2 n]
          (if (== 0 n)
            f2
            (recur f2 (+ f1 f2) (dec n))))]
    (cond
      (== n 0)
      0
      (== n 1)
      1
      :else
      (helper 0 1 (dec n)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [rev (reverse a-seq)]
    (reverse (map reverse (tails rev)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (let [n (max (count a-seq) 1)
        num-el (count a-seq)
        indices (range 0 num-el)
        drop-last (fn [s] (reverse (rest (reverse s))))
        rotate (fn [s]
                 (cons (last s) (drop-last s)))
        helper (fn [s rots-left ret]
                 (if (== rots-left 0)
                   ret
                   (recur (rotate s) (dec rots-left) (cons s ret))))]
      (helper a-seq num-el '()))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [el (first a-seq)]
      (if (contains? freqs el)
        (recur (assoc freqs el (inc (freqs el))) (rest a-seq))
        (recur (assoc freqs el 1) (rest a-seq))))) )


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [[el num-el] (first a-map)]
      (concat (repeat num-el el) (un-frequencies (rest a-map))))))



(defn my-take [n coll]
  (if (or (== n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (== n 0)
    coll
    (recur (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)
    b-seq
    (empty? b-seq)
    a-seq
    (< (first a-seq) (first b-seq))
    (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
    (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (empty? (rest a-seq)))
    a-seq
    (let [[a b] (halve a-seq)
          sa (merge-sort a)
          sb (merge-sort b)]
      (seq-merge sa sb))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [i (rest (inits a-seq))
          mono (last (take-while #(or (apply <= %)
                                      (apply >= %))
                                 i))]
      (cons mono
              (split-into-monotonics (drop (count mono) a-seq))))))



(defn permutations [a-seq]
  (cond
    (empty? a-seq)
    '(())
    (singleton? a-seq)
    [a-seq]
    :else
    (let [rots (rotations a-seq)]
      (apply concat (map (fn [rot] (map #(cons (first rot) %) (permutations (rest rot)))) rots)))))

(defn powerset [a-set]
  (if (empty? a-set)
    [a-set]
    (let [first-el (first a-set)
          rest-set (rest a-set)
          power-rest (powerset rest-set)]
      (concat
        (map #(cons first-el %) power-rest)
        power-rest))))

