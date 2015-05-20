(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (empty? (rest coll))
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (cond 
    (empty? a-seq) nil
    (< (count a-seq) 2) (first a-seq)
    :else  (apply max a-seq)
  ))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2
  ))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (= (count a-seq) 1) (first a-seq)
    :else (longest-sequence (conj (rest (rest a-seq)) (seq-max (first a-seq) (second a-seq))
      )
    )))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))
    )))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq))
    )))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '()
    )))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (not (== (count a-seq) (count b-seq))) false
    (empty? a-seq) true
    (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
  ))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons 
      (f (first seq-1) (first seq-2)) 
      (my-map f (rest seq-1) (rest seq-2)))
  ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))
  ))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (== up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond 
    (empty? a-seq) '([])
    :else
    (apply vector (cons a-seq (tails (apply vector (rest a-seq))))
    )))

(defn inits-apu [a-seq]
  (cond 
    (empty? a-seq) '([])
    :else
    (apply vector (cons a-seq (inits-apu (apply vector (butlast a-seq))))
    )))

(defn inits [a-seq]
  (apply vector (reverse (inits-apu a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map 
      (fn [x] 
        (concat
          (get (tails a-seq) x) 
          (get (inits a-seq) x))) 
      (my-range (count a-seq))
    )))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? (set (keys freqs)) (first a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))
    )))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [x] (my-repeat (get a-map x) x)) (keys a-map)))
)

(defn my-drop [n col]
  (if (< n 1)
    col
    (my-drop (dec n) (rest col))
  ))

(defn my-take [n coll]
  (reverse (my-drop (- (count coll) n) (reverse coll)))
)

(defn halve [a-seq]
  (if (< (count a-seq) 2)
    ['(), a-seq]
    (vector 
      (my-take (int (/ (count a-seq) 2)) a-seq)
      (my-drop (int (/ (count a-seq) 2)) a-seq)
    )))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))  '()
    (empty? a-seq) (cons (
        first b-seq) (seq-merge '() (rest b-seq))) 
    (empty? b-seq) (cons (
        first a-seq) (seq-merge '() (rest a-seq)))
    :else (if (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
      (cons (first b-seq) (seq-merge (rest b-seq) a-seq))
    )))

(defn merge-sort [a-seq]
  (cond
    (< (count a-seq) 2) a-seq
    :else (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq)))
    )))

(defn my-take-while2 [pred? a-seq viimeisin]
  "Ottaa kaksipaikkaisen predikaatin jonon perättäisten alkioiden vertailemiseen. Viimeisin-parametrin käyttö varmistaa ensimmäisen alkion käsittelemisen oikein."
  (if (< (count a-seq) 1)
    a-seq
    (if (pred? viimeisin (first a-seq))
      (cons (first a-seq) (my-take-while2 pred? (rest a-seq) (first a-seq)))
      '()
    )))

(defn my-drop-while2 [pred? a-seq viimeisin]
  "Vrt. my-take-while2"
  (if (< (count a-seq) 1)
    a-seq
    (if (pred? viimeisin (first a-seq))
      (my-drop-while2 pred? (rest a-seq) (first a-seq))
      a-seq
    )))

(defn split-into-monotonics-apuri [a-seq tulos]
  "Apuri kerryttää tulosta recursioiden yli."
  (cond
    (< (count a-seq) 1) tulos
    (= (count a-seq) 1) (conj tulos a-seq)
    (< (first a-seq) (second a-seq)) (split-into-monotonics-apuri
      (my-drop-while2 (fn [x y] (< x y)) a-seq (dec (first a-seq))) 
      (conj tulos (my-take-while2 (fn [x y] (< x y)) a-seq (dec (first a-seq)))))
    (> (first a-seq) (second a-seq)) (split-into-monotonics-apuri
      (my-drop-while2 (fn [x y] (> x y)) a-seq (inc (first a-seq))) 
      (conj tulos (my-take-while2 (fn [x y] (> x y)) a-seq (inc (first a-seq)))))
    :else (split-into-monotonics-apuri (rest a-seq)
      (conj tulos (seq [(first a-seq)])))
  ))

(defn split-into-monotonics [a-seq]
  (reverse (split-into-monotonics-apuri a-seq '()))
)

(defn permutations2 [a-set]
  (if (< (count a-set) 2 )
    (conj '() (seq a-set))
    (seq (set
        (map
          (fn [alkio] (map (fn [jono] (conj jono alkio)) (apply concat (rotations (permutations2 (disj a-set alkio))))))
          (seq a-set)
        )))
  ))

(defn permutations [a-set]
  (if (< (count a-set) 2 )
    (conj '() (conj '() (seq a-set)))
    (seq (set
        (map
          (fn [alkio] 
            (map 
              (fn [jono] (apply concat (conj jono alkio)))  
              (rotations (permutations (disj (set a-set) alkio)))))
          (seq a-set)
        ))
)
  ))

(defn powerset-apuri [tulos a-set]
  (if (empty? a-set)
    (clojure.set/union tulos #{#{}})
    (powerset-apuri
      (clojure.set/union
        tulos
        (set (map 
            (fn [x] (conj x (first (seq a-set)))) 
            (seq tulos))))
      (disj (set a-set) (first (seq a-set))) ;Rasittaa tuo "set a-set", kesti hetken huomata sen tarpeellisuus.
    )))

(defn powerset [a-set]
  (powerset-apuri #{#{}} a-set)
)
