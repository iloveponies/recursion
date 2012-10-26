(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (singleton? coll) (first coll) (if (empty? coll) nil (my-last (rest coll)))))

(defn max-element [a-seq]
  (let [rst (rest (rest a-seq))]
    (if (singleton? a-seq) (first a-seq) 
      (if (empty? a-seq) nil 
        (max-element (cons (max (first a-seq) (first (rest a-seq))) rst )))))) 

(defn seq-max [seq-1 seq-2]
  (let [fun (fn ! [x y] (if (empty? x) false
                        (if (empty? y) true (! (rest x) (rest y)))))]
    (if (fun seq-1 seq-2) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (let [rst (rest (rest a-seq))]
    (if (singleton? a-seq) (first a-seq) 
      (if (empty? a-seq) nil 
        (longest-sequence (cons (seq-max (first a-seq) (first (rest a-seq))) rst )))))) 

(defn my-filter [pred? a-seq]
  (let [rst (rest a-seq)
        [a] a-seq] 
    (if (singleton? a-seq) 
      (if (pred? a) [a] []) 
      (if (pred? a) (cons a (my-filter pred? rst)) (my-filter pred? rst)))))

(defn sequence-contains? [elem a-seq]
  (let [rst (rest a-seq)
        [a] a-seq]
    (if (empty? a-seq) false 
      (if (== elem a) true (sequence-contains? elem rst)))))

(defn my-take-while [pred? a-seq]
  (let [rst (rest a-seq)
        [a] a-seq]
    (if (empty? a-seq) []
      (if (pred? a) (cons a (my-take-while pred? rst)) []))))

(defn my-drop-while [pred? a-seq]
  (let [rst (rest a-seq)
        [a] a-seq]
    (if (empty? a-seq) [] 
      (if (pred? a) (my-drop-while pred? rst) a-seq))))

(defn seq= [a-seq b-seq]
  (let [rst-a (rest a-seq)
        [a] a-seq
        rst-b (rest b-seq)
        [b] b-seq
        ae? (empty? a-seq)
        be? (empty? b-seq)]
    (if (and ae? be?) true
      (if (or ae? be?) false
        (if (== a b) (seq= rst-a rst-b) false)))))

(defn my-map [f seq-1 seq-2]
  (let [rst-a (rest seq-1)
        [a] seq-1
        rst-b (rest seq-2)
        [b] seq-2
        ae? (empty? seq-1)
        be? (empty? seq-2)]
    (if (or ae? be?) []
      (cons (f a b) (my-map f rst-a rst-b)))))

(defn power [n k]
  (if (zero? k) 1
    (if (== k 1) n 
      (* n (power n (- k 1))))))

(defn fib [n]
  (if (== 0 n) 0
    (if (== 1 n) 1
      (+ (fib (- n 1)) (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) []
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0) []
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq) [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [f (fn a [x y] (if (zero? y) []
                       (cons x (a (#(concat (rest %) [(first %)]) x) (- y 1)))))]
    (f a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) {}
    (let [a (first a-seq)]
      (merge (my-frequencies-helper {} (my-filter #(not (= a %)) a-seq)) {a (count (my-filter #(= a %) a-seq))}))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) []
    (let [a (first a-map)
          rst (rest a-map)]
      (concat (repeat (val a) (key a)) (un-frequencies rst)))))

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])