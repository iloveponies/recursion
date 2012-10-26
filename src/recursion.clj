(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (product (rest coll)) (first coll))))

(defn singleton? [coll]
  (== (count coll) 1))

(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (if (empty? coll) nil (my-last (rest coll)))))

(defn max-element [a-seq]
  (let [sorted (sort a-seq)]
    (my-last sorted)))

(defn seq-max [seq-1 seq-2]
  (let [count1 (count seq-1)
        count2 (count seq-2)]
    (if (> count1 count2) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (let [rec (fn rec [a-seq max] (if (empty? a-seq)
                              max
                              (rec (rest a-seq) (seq-max max (first a-seq)))))]
    (rec a-seq nil)))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else []))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) []
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (let [fst (first a-seq)
        snd (first b-seq)]
    (cond
     (and (empty? a-seq) (empty? b-seq)) true
     (not (= fst snd)) false
     :else (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (let [fst (first seq-1)
        snd (first seq-2)]
    (if (or (empty? seq-1) (empty? seq-2))
      []
      (cons (f fst snd) (my-map f (rest seq-1) (rest seq-2))))))

(defn power [n k]
  (cond
   (= k 0) 1
   (= k 1) n
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (>= 0 up-to)
    []
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [rotate (fn rotate [seq] (if (empty? seq)
                                  []
                                  (concat (rest seq) (cons (first seq) []))))
        helper (fn helper [seq] (if (seq= a-seq seq)
                                      [a-seq]
                                      (cons seq (helper (rotate seq)))))]
    (helper (rotate a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
     freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (+ (get freqs (first a-seq)) 1)) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [elem (first (first a-map))
        times (second (first a-map))]
    (if (empty? a-map)
      []
      (concat (repeat times elem) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (let [fst (first coll)]
    (if (or (empty? coll) (<= n 0))
      []
      (cons fst (my-take (dec n) (rest coll))))))

(defn my-drop [n coll]
  (if (or (empty? coll) (<= n 0))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))
        pivot (- (count a-seq) half)]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a-fst (first a-seq)
        b-fst (first b-seq)]
    (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (<= a-fst b-fst) (cons a-fst (seq-merge (rest a-seq) b-seq))
     :else (cons b-fst (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (let [fst (first (halve a-seq))
        snd (second (halve a-seq))]
    (if (or (empty? a-seq) (singleton? a-seq))
      a-seq
      (seq-merge (merge-sort fst) (merge-sort snd)))))

(defn split-into-monotonics [a-seq]
  (let [h (fn [init] (or (apply <= init) (apply >= init)))]
    (loop
      [taken []
       seq a-seq]
      (let [new (count (take-while h (rest (inits seq))))]
        (if (empty? seq)
          taken
          (recur (conj taken (take new seq)) (drop new seq)))))))

(defn permutations [a-set]
  (let [rotate (fn rotate [seq] (if (empty? seq)
                                  []
                                  (concat (rest seq) [(first seq)])))
        rotate-n (fn [seq n]
                   (loop 
                     [acc [[seq]]
                      i n
                      s seq]
                     (if (<= i 0)
                       acc
                       (recur (cons (rotate s) acc) (dec i) (rotate s)))))
        permute-h (fn permute-h [seq n] 
                          (let [s seq]
                            (if (empty? s)
                              []
                              (concat (rotate-n (first s) n)
                                      (permute-h (rest s) n)))))
        permute (fn [seq] (permute-h seq (count seq)))]
    (cond
     (empty? a-set) [[]]
     (singleton? a-set) [[(first a-set)]]
      :else (permute (map (fn [x] (cons (first a-set) x)) 
                          (permutations (rest a-set)))))))

(defn powerset [a-set]
  [:-])