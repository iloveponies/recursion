(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll)
          (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (letfn [(max-element-n [a-seq n]
                         (cond
                           (= 0 (count a-seq)) n
                           (> (first a-seq) n) (max-element-n (rest a-seq) (first a-seq))
                           :else (max-element-n (rest a-seq) n)))]
    (if (= 0 (count a-seq))
      nil
      (max-element-n a-seq 0))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (let [n (count a-seq)
        long-seq (seq-max (first a-seq) (first (rest a-seq)))]
    (cond
      (= 0 n) nil
      (= 1 n) (first a-seq)
      :else (longest-sequence (cons long-seq (take-last (- n 2) a-seq))))))

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
  (letfn [(my-take-while-i [pred? a-seq i]
                          (cond
                           (empty? a-seq) 0
                           (pred? (first a-seq)) (my-take-while-i pred? (rest a-seq) (+ 1 i))
                           :else i))]
    (take (my-take-while-i pred? a-seq 0) a-seq)))

(defn my-drop-while [pred? a-seq]
  (letfn [(my-drop-while-n [pred? a-seq n]
                           (cond
                             (empty? a-seq) 0
                             (pred? (first a-seq)) (my-drop-while-n pred? (rest a-seq) (+ 1 n))
                             :else n))]
    (let [n (my-drop-while-n pred? a-seq 0)
          ret (take-last (- (count a-seq) n) a-seq)]
      (take (count ret) ret))))

(defn seq= [a-seq b-seq]
  (= a-seq b-seq))

(defn my-map [f a-seq seq-2]
  (cond
    (empty? a-seq) a-seq
    (empty? seq-2) seq-2
    :else (cons (f (first a-seq) (first seq-2))
                (my-map f (rest a-seq) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (if (> n 1)
   (+ (fib (- n 1)) (fib (- n 2)))
   n))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (> how-many-times 1) (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))
    (= how-many-times 1) (cons what-to-repeat '())
    :else '()))

(defn my-range [up-to]
  (let [a-seq '()]
    (if (zero? up-to)
      a-seq
      (concat a-seq [(dec up-to)] (my-range (dec up-to))))))

(defn tails [a-seq]
  (if (seq a-seq)
    (cons (take-last (count a-seq) a-seq) (tails (rest a-seq)))
    (cons a-seq '())))

(defn inits [a-seq]
  (let [n (count a-seq)]
    (letfn [(inits-i [a-seq i]
                     (if (<= i n)
                       (cons (take i a-seq) (inits-i a-seq (+ i 1)))
                       nil))]
      (inits-i a-seq 0))))

(defn rotations [a-seq]
  (letfn [(rot [a-seq i]
    (let [r-seq (concat (rest a-seq) [(first a-seq)])]
      (if (< (+ i 1) (count r-seq))
        (concat [r-seq] (rot r-seq (inc i)))
        [r-seq])))]
    (if (empty? a-seq)
      [[]]
      (rot a-seq 0))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [el (first a-seq)
          freq (get freqs el)
          rec-freq (fn []
                     (if (nil? freq)
                       (assoc freqs el 1)
                       (assoc freqs el (+ 1 freq))))]
      (my-frequencies-helper (rec-freq) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [freqs (my-frequencies a-map)
        kk (keys a-map)
        vv (vals a-map)
        n (count vv)]
    (letfn [(iter [i]
                  (if (< i n)
                    (concat (iter (+ i 1)) (repeat (nth vv i) (nth kk i)))
                    nil))]
      (iter 0))))

(defn my-take [n coll]
  (let [max (count coll)]
    (letfn [(my-take-i [coll i]
                       (if (and (< i n) (< i max))
                         (cons (first coll) (my-take-i (rest coll) (+ i 1)))
                         '()))]
      (my-take-i coll 0))))

(defn my-drop [n coll]
  (let [max (count coll)]
    (letfn [(my-drop-i [coll i]
                       (cond
                         (>= i max) '()
                         (>= i n) coll
                         :else (my-drop-i (rest coll) (+ i 1))))]
      (my-drop-i coll 0))))

(defn halve [a-seq]
  (let [lo []
        hi []
        n (count a-seq)
        half (int (/ n 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (>= (first a-seq) (first b-seq)) (concat [(first b-seq)] (seq-merge a-seq (rest b-seq)))
    (< (first a-seq) (first b-seq)) (concat [(first a-seq)] (seq-merge (rest a-seq) b-seq))
    :else '()))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) a-seq
    (> (count a-seq) 2) (let [half (halve a-seq)] (seq-merge (merge-sort (first half)) (merge-sort (last half))))
    :else (let [[a b] a-seq]
            (cond
              (nil? a) [b]
              (nil? b) [a]
              (< a b) [a b]
              :else [b a]))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

