(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (= '() coll))
        (= '() (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll)) ;huomioitava tapaus [] jottei jää ikuiseen rekursioon
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)(count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
  a-seq
  (if (pred? (first a-seq))
    (cons (first a-seq) (my-filter pred? (rest a-seq)))
    (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (= (first a-seq) elem)
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
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
  (cond
    (or (empty? a-seq) (empty? b-seq)) ;testataan ensin, ovatko molemmat tyhjiä jos toinen on tyhjä. Vältetään ongelmat sekvenssin loppupuoliskon nil-arvojen kanssa
      (if (and (empty? a-seq) (empty? b-seq))
        true
        false
        )
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))




(defn my-map [f seq-1 seq-2]
    (if (or (empty? seq-1) (empty? seq-2))
      []
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))



(defn power [n k] ; n^k
  (if (zero? k)
    1
    (* n (power n (dec k)))))



(defn fib [n]
  (if (< n 2)
    n ; n = 0 -> palautetaan 0; n = 1 -> palautetaan 1
    (+ (fib (- n 1)) (fib (- n 2)))))




(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))



(defn my-range [up-to]
  (if (< up-to 1)
    []
    (cons (dec up-to) (my-range (dec up-to)))))



(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
   (cons  a-seq (tails (rest a-seq)))))



(defn inits [a-seq] ;käännetyn sekvenssin hännät = alkuperäisen alut käännettyinä
  (let [k-sec (reverse a-seq)
         kaannetyt-k-loput (tails k-sec)]
      (map reverse kaannetyt-k-loput)))





(defn rotations [a-seq]
  (let [alut-laskeva-pituus (inits a-seq)
        loput-laskeva-pituus (tails a-seq)
        loput-nouseva-pituus (reverse loput-laskeva-pituus)]

    (set (map concat  loput-nouseva-pituus alut-laskeva-pituus))))



(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [alkio (first a-seq)
          uusi-maara (if (contains?  freqs alkio)
                                    (inc (get freqs alkio))
                                    1)
          uusi-freqs (assoc freqs alkio uusi-maara)]
      (my-frequencies-helper uusi-freqs (rest a-seq)))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


  (my-frequencies []); => {}
  (my-frequencies [1 1 2 2 :D :D :D]) ;=> {1 2, 2 2, :D 3}
  (my-frequencies [:a "moi" :a "moi" "moi" :a 1])
  ;=> {:a 3, "moi" 3, 1 1})


(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [pari (first a-map)
          toistettava (first pari)
          lkm (second pari)
          yhdistettava (repeat lkm toistettava)]
      (concat yhdistettava (un-frequencies(rest a-map))))))



(defn my-take [n coll]
   (if (or (zero? n) (empty? coll))
     []
     (cons (first coll) (my-take (dec n) (rest coll)))))


(defn my-drop-helper [k n coll]
  (cond
    (empty? coll)
      []
    (< k n)
      (my-drop-helper (inc k) n (rest coll))
    :else
      (cons (first coll) (my-drop-helper (inc k) n (rest coll))))) ;oikeastaan k:n korottaminen turhaa ehdon toteutumisen jälkeen...

(defn my-drop [n coll]
  (my-drop-helper 0 n coll))



(defn halve [a-seq]
  (let [n (count a-seq)
        alun-pituus (int (/ n 2))
        alku  (my-take alun-pituus a-seq)
        loppu (my-drop alun-pituus a-seq)]
    [alku loppu]))



(defn seq-merge [a-seq b-seq]
  (cond                       ;2 tapausta, olisihan se if riittänyt...
    (or (empty? a-seq) (empty? b-seq))
      (if (and (empty? a-seq) (empty? b-seq))
        []
        (if (empty? a-seq)
          (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
          (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
          )
        )

    :else
      (if (< (first a-seq) (first b-seq))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq) )
        (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))



(defn merge-sort [a-seq]
 ( if (or (empty? a-seq) (singleton? a-seq))
     a-seq
     (let [[alku loppu]  (halve a-seq)
           jarjestetty-alku (merge-sort alku)
           jarjestetty-loppu (merge-sort loppu)]
       (seq-merge jarjestetty-alku jarjestetty-loppu))))




(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
)




 (my-drop-while odd? [1 2 3 4])  ;=> '(2 3 4)
  (my-drop-while odd? [1 3 4 5])  ;=> '(4 5)
  (my-drop-while even? [1 3 4 5]) ;=> '(1 3 4 5)
  (my-drop-while odd? [])        ;=> empty?)
