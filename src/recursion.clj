(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and ((complement empty?) coll) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) ((complement pred?) (first a-seq)))
    '()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (or (empty? a-seq) (empty? b-seq)) (= a-seq b-seq)
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (= n 1) 1
   (neg? n) nil
   :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (pos? up-to)
    (cons (dec up-to) (my-range (dec up-to)))
    '()))

(defn tails [a-seq]
  (cond
   (empty? a-seq) (cons '() '())
   :else (cons (cons (first a-seq) (rest a-seq))
               (tails (rest a-seq)))))

(defn inits [a-seq]
  (cond
   (empty? a-seq) (cons '() '())
   :else (cons (cons (first a-seq) (rest a-seq))
               (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (let [eka (first a-seq)
        vika (last a-seq)
        muut (butlast a-seq)
        elems (count a-seq)]
  (cond
   (empty? a-seq) '(())
   (and (not (sequential? eka)) (< elems 2)) (cons a-seq '())
   (not (sequential? eka)) (rotations (cons (cons eka (rest a-seq)) '()))
   (< elems (count eka)) (rotations (concat (cons (cons (last eka) (butlast eka)) '())  a-seq))
   :else a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (let [eka (first a-seq)
        loput (rest a-seq)]
    (cond (empty? a-seq) freqs
          (contains? freqs eka) (my-frequencies-helper (assoc freqs eka (inc (freqs eka))) loput)
          :else (my-frequencies-helper (assoc freqs eka 1) loput))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [result a-map]
  (let [eka-avain (first (keys a-map))]
    (if (empty? a-map)
      result
      (un-frequencies-helper (concat (repeat (second (first a-map)) eka-avain) result)
                             (dissoc a-map eka-avain)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper '() a-map))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (and (pos? n) ((complement empty?) coll))
    (my-drop (dec n) (rest coll))
    coll))

(defn halve [a-seq]
  (let [alku (int (/ (count a-seq) 2))]
    (concat (cons (my-take alku a-seq) '()) (cons (my-drop alku a-seq) '()))))

(defn seq-merge [a-seq b-seq]
  (let [a-eka (first a-seq)
        b-eka (first b-seq)
        a-lop (rest a-seq)
        b-lop (rest b-seq)]
    (cond
     (and (empty? a-seq) (empty? b-seq)) '()
     (empty? b-seq) (cons a-eka (seq-merge a-lop b-seq))
     (empty? a-seq) (cons b-eka (seq-merge a-seq b-lop))
     (< a-eka b-eka) (cons a-eka (seq-merge a-lop b-seq))
     :else (cons b-eka (seq-merge a-seq b-lop)))))

(defn merge-sort [a-seq]
  (cond
   (< (count a-seq) 2) a-seq
   :else (apply seq-merge (map merge-sort (halve a-seq)))))

(defn select-biggest-monotonous [inits-seq grow?]
  (let [isoin (first inits-seq)]
    (cond
     (< (count inits-seq) 4) isoin
     (and grow? (apply < isoin)) isoin
     grow? (select-biggest-monotonous (rest inits-seq) grow?)
     (apply > isoin) isoin
     :else (select-biggest-monotonous (rest inits-seq) grow?))))

(defn split-into-monotonics [a-seq]
  (let [inits-seq (inits a-seq)
        ekat (first inits-seq)
        kasv (select-biggest-monotonous inits-seq true)
        lask (select-biggest-monotonous inits-seq false)]
    (cond
     (empty? a-seq) '()
     (< (count a-seq) 3) (cons a-seq '())
     (> (second ekat)
        (first ekat)) (concat (cons kasv '())
                              (split-into-monotonics (my-drop (count kasv) a-seq)))
     :else (concat (cons lask '())
                   (split-into-monotonics (my-drop (count lask) a-seq))))))

(defn perm-solver [kohta rotats]
  (let []
     (cond
      (>= (inc kohta) (count rotats)) rotats
      :else (map (fn [x] (perm-solver (inc kohta) x)) (map (fn [x] (concat (my-take kohta rotats) x))
                                  (rotations (my-drop kohta rotats)))))))

(defn get-subseqs [n a-seq]
  (if (pos? n)
    (get-subseqs (dec n) (apply concat a-seq))
    a-seq))

(defn permutations [a-set]
  (if (< (count a-set) 3)
    (rotations a-set)
    (get-subseqs (- (count (perm-solver 0 a-set)) 2) (perm-solver 0 a-set))))

(defn powerset-helper [res a-set]
  (if (empty? a-set)
    res
    (reduce powerset-helper
            (conj res a-set)
            (map (partial disj a-set) a-set))))


(defn powerset [a-set]
  (powerset-helper #{#{}} (set a-set)))

