(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll)) (not (empty? coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else
        (sequence-contains? elem (rest a-seq))
        ))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (cons (first a-seq)(my-take-while pred? (rest a-seq)))
   :else
     []))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq))

(defn seq= [a-seq b-seq]
  (if (= (count a-seq) (count b-seq))
    (cond
     (and (empty? a-seq) (empty? b-seq)) true
     (or (empty? a-seq) (empty? b-seq)) false
     (not (= (first a-seq) (first b-seq))) false
     :else (seq= (rest a-seq) (rest b-seq)))
    false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) []
   :else
   (cons (f (first seq-1) (first seq-2))
         (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? n) 0
   (zero? k) 1
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0) []
   (== how-many-times 1) [what-to-repeat]
   :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [k (dec up-to)]
   (cond  (< up-to 1) []
          (= up-to 1) [k]
          :else (cons k (my-range k)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons [] a-seq)
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [element (first a-seq)
          new-freqs (if (contains? freqs element)
                      (update-in freqs [element] inc)
                      (assoc freqs element 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [first-elem (first a-map)
        last-elem (rest a-map)
        k (if (empty? a-map)
            []
            (my-repeat (val first-elem) (key first-elem)))]
    (cond
     (empty? a-map) []
          (singleton? a-map) k
          :else (concat k (un-frequencies last-elem)))))

(defn my-take [n coll]
  (cond
   (empty? coll) coll
   (zero? n) []
   :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [halve-cnt (int (/ (count a-seq) 2))]
      (vector (my-take halve-cnt a-seq)
              (my-drop halve-cnt a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) '()
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[first-halve, second-halve] (halve a-seq)]
      (seq-merge (merge-sort first-halve) (merge-sort second-halve)))))

(defn split-into-monotonics [a-seq]
  (let [initrev (reverse (inits a-seq))
        pred (fn [k] (or (apply < k) (apply > k)))
        f-mono (last (take-while pred (drop 2 initrev)))]
    (if (<= (count a-seq) 1)
      a-seq
      (cons f-mono (split-into-monotonics (drop (count f-mono) a-seq))))))

(defn permutations [a-set]
  (if (or (empty? a-set) (singleton? a-set))
    (list a-set)
    (mapcat
     #(map (fn [k] (cons (first %) k)) (permutations (rest %)))
     (rotations a-set))))

(defn powerset [a-set]
  (if (empty? a-set)
    '#{#{}}
    (cons (set a-set) (set (apply concat (map powerset (map rest (rotations a-set))))))))

