(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))
;;enta jos coll on valmiiksi tyhja?
;;huomioitava itse koodissa, silla talloin
;;saa arvon 1 (voinee johtaa virheeseen muuten)

(defn singleton? [coll]
  (and
   (not (empty? coll))
   (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
       (first a-seq)
       (max
        (first a-seq)
        (max-element
         (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if
    (>
     (count seq-1)
     (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max
    (first a-seq)
    (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))
;;jos funktio pred? saa arvon true ensimmaiselle arvolle,
;;yhdistaa ensimmaisen filteroituun loppulistaan, i.e.
;;muodostaa luupin joka loppuu kun lista on kayty lapi, kunnes
;;viimeisena lista on tyhja ja filteroitu lista yhdistetaan
;;tyhjaan listaan

(defn sequence-contains? [elem a-seq]
  (if
   (empty? a-seq) false
   (if
     (= elem (first a-seq)) true
     (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if
    (empty? a-seq) ()
      (if (pred? (first a-seq))
        (cons (first a-seq)
              (my-take-while pred? (rest a-seq)))
        ())))
;;kun jalkimmainen if-lause arvioidaan viimeisen kerran ja
;;saadaan arvoksi (), lisataan se edellisen luupin arvion saamaan
;;arvoon :)

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
    ()
   (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
   :else a-seq))
;;rekursiivinen ajattelu! a-seq on vain nimitys funktiolle
;;annetulle sekvenssille, i.e. tassa tapauksessa alkuperainen
;;sekvenssi kutistuu aina yhdella
;;seuraavaan luupin kierrokseen edetessa

(defn seq= [a-seq b-seq]
   (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
     :else false))

(defn my-map [f seq-1 seq-2]
  (let [a (first seq-1)
        b (first seq-2)
        r-a (rest seq-1)
        r-b (rest seq-2)]
    (if (or (empty? seq-1) (empty? seq-2))
      ()
      (cons
       (f a b) (my-map f r-a r-b)))))

(defn power [n k]
  (if (> k 0)
    (* n (power n (- k 1)))
    1))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   (>= n 2) (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (let [h how-many-times
        w what-to-repeat]
    (cond
     (> h 0)
      (cons w (my-repeat (- h 1) w))
     :else ())))

(defn my-range [up-to]
  (let [u up-to]
    (if (zero? u) ()
      (cons (dec u) (my-range (dec u))))))

(defn tails [a-seq]
  (if (empty? a-seq) [[]]
    (let [u (count a-seq)]
    (if
      (>= u 1)
      (cons a-seq (tails (rest a-seq)))
      [[]]))))
;;pelkka if emptykin olisi tosin riittava

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))
;;annetaan tailsille jono vaarinpain, i.e. sama kuin edelle mutta
;;poistaen viimeisen arvon, taman jalkeen kaannetaan kaikki jonot
;;halutun suuntaisiksi ja jarjestetaan viela kaikki nousevaan
;;jarjestukseen

(defn rotations [a-seq]
  (if (empty? a-seq) [()]
  (rest (map concat (tails a-seq) (inits a-seq)))))
;;ensimmainen tulos poistettava, koska inits ja tails sisaltavat
;;molemmat tyhjan joukon, toisin sanoen yhdistaessa ne painvastaisessa
;;jarjestyksessa, tulee jono itsessaan toistettua kerran

;;laskee yksittaisen alkion frekvenssin jonossa
;;tarkistaa tyhja, ja sisaltaako freqs jo haluttua arvoa
;;jollei sisalla, assosioi halutun arvon sen lukumaaralla a-seq:ssa
;;lukumaara saadaan filteroimalla jonosta kaikki muut arvot pois ja
;;laskemalla jaljelle jaaneen jonon pituus
;;tosin, olisikohan ollut nopeampaa vain verrata jokaista arvoa
;;freqsiin ja kasvattaa aina kyseisen arvon lukumaaraa yhdella
;;suurella jonolla kun voi vieda kauan tyhjentaa muut arvot jonosta ja
;;heti peraan laskea jaljelle jaaneiden lukumaara...
(defn my-frequencies-helper [freqs a-seq]
  (cond
   (empty? a-seq) freqs
   (contains? freqs (first a-seq))
     (my-frequencies-helper freqs (rest a-seq))
   :else (my-frequencies-helper
      (assoc freqs (first a-seq) (count (my-filter (fn [value] (= (first a-seq) value)) a-seq)))
      (rest a-seq))))

(defn my-frequencies [a-seq]
 (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (concat (my-repeat (val (first a-map)) (key (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond
   (empty? coll) coll
   (> n 0) (cons (first coll) (my-take (dec n) (rest coll)))
   :else ()))

(defn my-drop [n coll]
    (cond
     (empty? coll) coll
     (zero? n) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [halfway (int (/ (count a-seq) 2))]
  [(my-take halfway a-seq) (my-drop halfway a-seq)]))

(defn seq-merge [a-seq b-seq]
     (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      :else (if (<= (first a-seq) (first b-seq))
              (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
              (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
   )))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) a-seq
   (= (count a-seq) 1) a-seq
   :else (let
           [[a2 b2] (halve a-seq)]
           (seq-merge (merge-sort a2) (merge-sort b2)))))

(defn split-into-monotonics [a-seq]
  ())

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

