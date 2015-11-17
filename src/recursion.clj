(ns recursion)

(defn product [coll]
   (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
   (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
 (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (let [singleton-or-empty (or (singleton? a-seq) (empty? a-seq))]
   (if singleton-or-empty
     (first a-seq)
     (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [first-count (count seq-1)
        seconf-count (count seq-2)
        first-is-bigger (< seconf-count first-count)]
  (if  first-is-bigger seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (let [singleton-or-empty (or (singleton? a-seq) (empty? a-seq))]
   (if singleton-or-empty (first a-seq)
     (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
      a-seq
      (if (pred? (first a-seq))
          (cons (first a-seq) (my-filter pred? (rest a-seq)))
          (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [not-empty (not (empty? a-seq))
        frst (first a-seq)
        rst  (rest a-seq)]
  (if (and not-empty (pred? frst))
    (cons frst (my-take-while pred? rst))
    ())))

(defn my-drop-while [pred? a-seq]
   (let [not-empty (not (empty? a-seq))
        frst (first a-seq)
        rst  (rest a-seq)]
  (if (and not-empty (pred? frst))
    (my-drop-while pred? rst)
    a-seq)))

(defn seq= [a-seq b-seq]
  (let [a-count (count a-seq)
        b-count (count b-seq)
        size-equals (= a-count b-count)
        a-empty (empty? a-seq)
        b-empty (empty? b-seq)
        both-empty (and a-empty b-empty)]
  (if size-equals
    (if both-empty true
      (if (= (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))
        false))
  false)))

(defn my-map [f seq-1 seq-2]
  (let [first-empty (empty? seq-1)
        second-empty (empty? seq-2)
        one-of-them-is-empty (or first-empty  second-empty)
        one-of-them-is-singleton (or (singleton? seq-1) (singleton? seq-2))]
  (if one-of-them-is-empty ()
    (if one-of-them-is-singleton
    (cons (f (first seq-1) (first seq-2)) ())
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))))

(defn power [n k]
  (if (< 0 k)  (* n (power n (dec k))) 1))

(defn fib [n]
  (if (or (== n 0) (== n 1))
    (if (== n 0) 0 1)
    (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< 0 how-many-times)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
                                                  ()))

(defn my-range [up-to]
  (if (< 0 up-to)
    (cons (dec up-to) (my-range (dec up-to)))
    ()))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
 (if (empty? a-seq)
   [()]
   (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
  (let [x (first a-seq)
      new-val (if (freqs x) (+ 1 (freqs x)) 1)]
    (my-frequencies-helper (assoc freqs x new-val) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [[a-key a-val] (first a-map)]
      (concat (repeat a-val a-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (and (< 0 n) (not (empty? coll)))
    (cons (first coll) (my-take (dec n) (rest coll)))
    ()))

(defn my-drop [n coll]
  (if (and (< 0 n) (not (empty? coll)))
    (my-drop (dec n) (rest coll))
    coll))

(defn halve [a-seq]
 (let [h (int (/ (count a-seq) 2))]
  [(take h a-seq) (drop h a-seq)]))

(defn seq-merge [a-seq b-seq]
   (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (< (first a-seq) (first b-seq))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
       :else
        (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
       (if (or (empty? a-seq) (empty? (rest a-seq)))
        a-seq
        (apply seq-merge (map merge-sort (halve a-seq)))))

(defn grab-rest [grabbed dir a-seq]
  (cond
    (empty? a-seq) [(reverse grabbed) a-seq]
    (empty? grabbed) (if (empty? (rest a-seq))
                      [a-seq (rest a-seq)]
                      (let [fst (first a-seq)
                        snd (second a-seq)
                        rst (rest (rest a-seq))
                        dir (cond
                              (< fst snd) <=
                              (> fst snd) >=
                              :else nil)]
                        (grab-rest [(second a-seq) fst] dir rst)))
      dir (if (dir (first grabbed) (first a-seq))
            (grab-rest (cons (first a-seq) grabbed) dir (rest a-seq))
            [(reverse grabbed) a-seq])
      :else (let [dir (cond
                        (< (first a-seq) (first grabbed)) >=
                        (> (first a-seq) (first grabbed)) <=
                        :else nil)]
            (grab-rest (cons (first a-seq) grabbed) dir (rest a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [[grabbed rst] (grab-rest [] nil a-seq)]
      (cons grabbed (split-into-monotonics rst)))))

(defn permutations [a-set]
  (if (<= (count a-set) 1)
    (seq [a-set])
    (let [perms (permutations (rest a-set))
      glue (fn [x y] (concat x [(first a-set)] y))
      inserts (fn [perm] (map glue (inits perm) (tails perm)))]
    (apply concat (map inserts perms)))))

(defn powerset [a-set]
  (if (< (count a-set) 1)
    (seq [a-set])
    (let [powers (powerset (rest a-set))
      with (map (fn [pset] (cons (first a-set) pset)) powers)]
      (concat with powers))))
