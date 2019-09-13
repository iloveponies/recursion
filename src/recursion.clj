(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (let [fst (first a-seq)]
    (cond
      (empty? a-seq) nil
      (singleton? a-seq) fst
      :else (max fst (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [count-1 (count seq-1)
        count-2 (count seq-2)]
    (if (> count-1 count-2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (let [fst (first a-seq)]
    (cond
      (empty? a-seq) nil
      (singleton? a-seq) fst
      :else (seq-max fst (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (let [fst (first a-seq)
        rst (rest a-seq)]
    (if (empty? a-seq)
      a-seq
      (if (pred? fst)        
        (cons fst (my-filter pred? rst))
        (my-filter pred? rst)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (or (= elem (first a-seq))
        (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (let [fst (first a-seq)]
    (if (empty? a-seq)
      a-seq
      (if (pred? fst)
        (cons fst (my-take-while pred? (rest a-seq)))
        ()))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond    
    (< n 1) 0
    (= n 1) 1
    (= n 2) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (let [up-to-1 (- up-to 1)]
    (if (< up-to 1)
      ()
      (cons up-to-1 (my-range up-to-1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (vector a-seq) ; (tails '()) => ([]) and not (()) :(
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (let [rotate (fn [seq n] (concat (drop n seq) (take n seq)))
        func (fn [x] (rotate a-seq x))]
    (if (empty? a-seq)
      (vector a-seq)
      (map func (range (count a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (let [fst (first a-seq)
        current-freq (get freqs fst)
        do-rest (fn [x] (my-frequencies-helper x (rest a-seq)))]
    (if (empty? a-seq)
      freqs
      (if (nil? current-freq)
        (do-rest (assoc freqs fst 1))
        (do-rest (assoc freqs fst (+ current-freq 1)))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [func (fn [x] (repeat (val x) (key x)))]
    (flatten (map func a-map))))

(defn my-take [n coll]
  (if (or (empty? coll)
          (< n 1))
    ()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll)
          (< n 1))
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [length (count a-seq)
        center (/ length 2)]
    (vector (my-take center a-seq) (my-drop center a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [a-fst (first a-seq)
        b-fst (first b-seq)
        a-rst (rest a-seq)
        b-rst (rest b-seq)]
    (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      :else (if (< a-fst b-fst)
              (cons a-fst (seq-merge a-rst b-seq))
              (cons b-fst (seq-merge a-seq b-rst))))))

(defn merge-sort [a-seq]
  (let [[a-s, b-s] (halve a-seq)]
    (if (or (empty? a-seq)
            (singleton? a-seq))
      a-seq
      (seq-merge (merge-sort a-s) (merge-sort b-s)))))

(defn index-of-change [f start a-seq]
  (loop [strt start
         rst a-seq
         ind 0]
    (cond
      (empty? rst) -1
      (not (f strt (first rst))) ind
      :else (recur (first rst) (rest rst) (inc ind)))))

(defn split-into-monotonics [a-seq]  
  (loop [s a-seq ; s = fst : snd : rst
         result []]
    (let [fst (first s)
          snd (second s)
          rst (rest (rest s))
          order (if (or (nil? fst) (nil? snd))
                  nil ;(fn [x] false)
                  (if (<= fst snd) <= >=)) ; <= >= ?
          index-of-change (index-of-change order snd rst)]
      (cond
        (nil? fst) result
        (nil? snd) (concat result [[fst]])
        (empty? rst) (concat result [[fst snd]])
        (= index-of-change -1) (concat result s)
        :else (recur
                (drop index-of-change rst)
                (conj result (concat [fst snd] (take index-of-change rst))))))))

(defn without [s i]
  (concat (take i s) (drop (+ i 1) s)))

(defn permutations [a-set]
  (let [a-seq (seq a-set)
        func (fn [i]
          (let [curr (nth a-seq i)
                others (without a-seq i)]
             (map (partial cons curr) (permutations others))))
        indices (range (count a-seq))]
    (cond
      (empty? a-seq) (vector (vector))
      (singleton? a-seq) (vector a-seq)
      :else (reduce concat (map func indices)))))

(defn powerset [a-set]
  (let [setto (set a-set)
        indices (range (count setto))]    
    (set
      (if (empty? setto)
          (vector setto)
          (cons
            setto
            (reduce concat (map (fn [x] (powerset (disj setto x))) setto)))))))
