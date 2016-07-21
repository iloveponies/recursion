(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)) )))

(def length
 (fn [lst]
  (loop [i lst cnt 0]
   (cond (empty? i) cnt
     :t (recur (rest i)(inc cnt))))))

((defn singleton? [coll]
  (if (= 1 (length coll)) true false)) [nil])

(defn my-last [coll]
  (if (or (= 0 (length coll)) (singleton? coll))
    (first coll)
    (my-last (disj (into #{} coll) (first coll)) ) ))


(defn max-element-helper [maximi a-seq]
  (if (= 0 (length a-seq))
    maximi
    (max-element-helper (max (first a-seq) maximi) (disj a-seq (first a-seq))) ))


(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (max-element-helper (first a-seq) (disj (into #{} a-seq) (first a-seq)) ) ))

(defn seq-max [seq-1 seq-2]
  (if (< (length seq-2) (length seq-1))
    seq-1 seq-2))

(defn max-seq-helper [maximi a-seq]
  (if (= 0 (length a-seq))
    maximi
    (max-seq-helper (seq-max (first a-seq) maximi) (disj a-seq (first a-seq))) ))


(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (max-seq-helper (first a-seq) (disj (into #{} a-seq) (first a-seq)) ) ))


(defn my-filter-helper [pred? palautus a-seq]
   (if (= 0 (length a-seq))
    (into [] palautus)
    (my-filter-helper pred? (if(pred? (first a-seq)) (conj palautus (first a-seq)) palautus) (rest a-seq) )))

(defn my-filter [pred? a-seq]
  (my-filter-helper pred? #{} (into #{} a-seq)))

(defn sequence-contains? [elem a-seq]
   (cond
   (empty? a-seq)
     false

   (= elem (first a-seq))
     true

   :else
     (sequence-contains? elem (rest a-seq) ) ))

(defn all-true [pred? a-seq]
  (if (= (length a-seq) (length (filter boolean (map pred? a-seq)))) true false))


(defn longest-array [pred? a-seq current-prefix] (let [current-prefix-plus-eka (conj current-prefix (first a-seq))]
   (if (= 0 (length a-seq))
    (into [] current-prefix)
      (if (all-true pred? current-prefix-plus-eka)
              (longest-array pred? (rest a-seq) current-prefix-plus-eka )
               current-prefix )
              )))


((defn my-take-while [pred? a-seq]
   (longest-array pred? a-seq []) ) pos? [-2 -3 2] )


(defn my-drop-while [pred? a-seq]
  (if (and (< 0 (length a-seq)) (pred? (first a-seq))) (my-drop-while pred? (rest a-seq)) a-seq))

((defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq)) true  (if (and (= (length a-seq) (length b-seq)) (= (first a-seq) (first b-seq))) (seq= (rest a-seq) (rest b-seq)) false) )) [1 2 nil] [1 2])



(defn my-map-helper [f seq-1 seq-2 tulos]
  (if (or (empty? seq-1) (empty? seq-2)) tulos
    (my-map-helper f (rest seq-1) (rest seq-2) (conj tulos (f (first seq-1) (first seq-2))) )))

(defn my-map [f seq-1 seq-2]
  (my-map-helper f seq-1 seq-2 []))

(defn power [n k]
  (if (= k 0) 1 (* n (power n (- k 1)))))

(defn fib [n]
 (cond
    (= n 1) 1
    (= n 0) 0
    (< n 0) -1
    (= n 2) 1
    :else (+ (fib (- n 1)) (fib (- n 2))) ))



(defn my-repeat-helper [how-many-times what-to-repeat lista]
  (if (>= 0 how-many-times) lista (my-repeat-helper (- how-many-times 1) what-to-repeat (conj lista what-to-repeat)) ))

(defn my-repeat [how-many-times what-to-repeat]
  (my-repeat-helper how-many-times what-to-repeat []))

(defn my-range-helper [up-to lista]
  (if (= up-to -1) lista (my-range-helper (- up-to 1) (conj lista up-to))))

(defn my-range [up-to]
  (my-range-helper (- up-to 1) []))


(defn tails-helper [a-seq kokolista]
  (if (empty? a-seq) (conj kokolista a-seq) (tails-helper (rest a-seq) (conj kokolista a-seq))))

(defn tails [a-seq]
  (tails-helper a-seq []))

(defn inits-helper [a-seq kokolista]
  (if (empty? a-seq) (conj kokolista a-seq) (inits-helper (rest a-seq) (conj kokolista (reverse a-seq)))))

(defn inits [a-seq]
  (inits-helper (reverse a-seq) []))

(defn rotations-helper [rotationslist currentrotation]
  (if (= (count currentrotation) (count rotationslist)) rotationslist (rotations-helper (conj rotationslist currentrotation) (concat [(last currentrotation)] (drop-last currentrotation)) )))


(defn rotations [a-seq]
 (if (empty? a-seq) (conj [] []) (rotations-helper () a-seq)))


(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))

(defn my-frequencies-helper [freqs a-seq real-a-seq]
  (if (empty? a-seq) freqs
    (my-frequencies-helper (assoc freqs (first a-seq) (count-elem-helper 0 (first a-seq) real-a-seq)) (rest a-seq) real-a-seq) ))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq a-seq))


(defn un-frequencies-helper [a-map unfreqs] ( let [[key value] (first a-map)]
  (if (empty? a-map) unfreqs
    (un-frequencies-helper (rest a-map) (concat unfreqs (repeat value key))) )))

(defn un-frequencies [a-map]
  (un-frequencies-helper a-map []))

(defn my-take [n coll]
  (drop-last (- (count coll) n) coll))

((defn my-drop [n coll]

  (if (< (count coll) n) []
  (reverse (my-take n (reverse coll))))) 4 [1 2 3 4 5])

((defn halve [a-seq]
  (conj (conj [] (my-take (/ (count a-seq) 2) a-seq)) (my-drop  (int (Math/ceil (/ (count a-seq) 2))) a-seq)) ) [1 2 3 4 5])


(defn seq-merge-helper [a-seq b-seq yhdistys]
  (cond
    (empty? a-seq) (concat yhdistys b-seq)
    (empty? b-seq) (concat yhdistys a-seq)
    (< (first a-seq) (first b-seq)) (seq-merge-helper (rest a-seq) b-seq (conj yhdistys (first a-seq)))
    (> (first a-seq) (first b-seq)) (seq-merge-helper a-seq (rest b-seq) (conj yhdistys (first b-seq)))
    (= (first a-seq) (first b-seq)) (seq-merge-helper (rest a-seq) (rest b-seq) (conj yhdistys (first b-seq) (first b-seq)))
     ))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq []))

((defn merge-sort [a-seq]
   ( if (>= 1 (length a-seq)) a-seq
  (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))  [1 3 2])


(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

