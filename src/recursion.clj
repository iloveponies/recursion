(ns recursion)

(defn product
  "Kertoma kokoelmalle"
  [coll]
  (if (empty? coll)
        1
        (* (first coll)
           (product (rest coll)))))

 ;=> (product [1 2 4]
 ;=> (* 1 (product (rest coll)))
 ;=> (* 1 (* 2 (product (rest coll))))
 ;=> (* 1 (* 2 (* 4 (product (rest coll)))))
 ;=> (* 1 (* 2 (* 1 4))) ; (empty? coll) on true, siksi 1
 ;=> (* 1 (* 2 4))
 ;=> (* 1 8)
 ;=> 8

(defn singleton?
  "Tarkistaa onko kokoelma yhden kokoinen"
  [coll]
  (if (empty? coll)
        false
        (= (rest coll) '())))

(defn my-last
  "Palauttaa kokoelma viimeisen elementin"
  [coll]
  (if (empty? coll)
      nil
      (if (singleton? coll)
          (first coll)
           (my-last (rest coll)))))

(defn max-element
  "Palattaa kokoelman suurimman elementin"
  [a-seq]
  (cond
     (empty? a-seq) nil
     (singleton? a-seq) (first a-seq)
     :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max
  "Palauttaa suuremman sekvenssin"
  [seq-1 seq-2]
   (let [maxx (cond
                 (empty? seq-1) 2
                 (empty? seq-2) 1
                 :else (seq-max (rest seq-1) (rest seq-2)))]
       (if (= maxx 1)
           seq-1
           seq-2)))


(defn longest-sequence
  "Palauttaa sekvenssin suurimman sekvenssin"
  [a-seq]
  (cond
      (empty? a-seq) nil
      (singleton? a-seq) (first a-seq)
      :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter
  "Palauttaa annetusta sekvenssistä ne elementit, joilla pred? pätee. Mun aivoihin sattuu"
  [pred? a-seq]
  (if (empty? a-seq)
      a-seq
      (if (pred? (first a-seq))
                (cons (first a-seq) (my-filter pred? (rest a-seq)))
                (my-filter pred? (rest a-seq)))))

; (my-filter odd? [1 2 3 4])
; (cons 1 (my-filter odd? (rest a-seq))) ; rest a-seq = [2 3 4]
; (cons 1 (my-filter odd? (rest a-seq))) ; rest a-seq = [3 4]
; (cons 1 (cons 3 (my-filter odd? ([4]))))
; (cons 1 (cons 3 (my-filter odd? ([]))))
; (cons 1 (cons 3 (a-seq))) ; koska if empty?
; (cons 1 (cons 3 []))
; [1 3]

(defn sequence-contains? [elem a-seq]
    (cond
      (empty? a-seq) false
      (= elem (first a-seq)) true
      :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while
  "Palautetaan alusta lähtien kaikki perättäiset elementit joilla pred? pätee"
  [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) '() 
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while
  "Vähennetään elementtejä alusta lähtien siihen astin kunnes pred? palauttaa false ensimmäisen kerran"
  [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq=
  "Tarkastaa ovatko kaksi sekvenssiä samat"
  [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map
  "Syötetään funktiolle f elementti kummastakin sekvenssistä. Kerätään f:n tulokset uuten sekvenssiin. Lopetetaan, kun seq-1 tai seq-2 on käyty loppuun"
  [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power
  "Lasketaan n potenssiin k"
  [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib
  "Lasketaan fibonaccin luku luvulle n"
  [n]
  (cond
    (= n 0) n
    (= n 1) n
    (= n 2) (+ (- n 1) (- n 2))
    :else (+ (fib (- n 1)) (fib (- n 2)))))


(defn my-repeat
  "Toistetaan what-to-repeat how-many-times kertaa"
  [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) []
    :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range
  "Muodostetaan monotonisesti laskeva sekvenssi joka lähtee luvusta up-to-1"
  [up-to]
  (cond
    (= up-to 0) '()
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails
  "Poimitaan sekvenssiin a-seq alasarjoja lopusta katsoen"
  [a-seq]
  (cond
    (empty? a-seq) '([])
    :else (cons a-seq (tails (rest a-seq)))))

(defn inits
   "Poimitaan sekvenssin a-seq alasarjoja lopusta katsoen"
  [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations-helper 
  "Rotaatio-apulainen"
  [in a-seq]
;  (let [rotten (concat (rest a-seq) 
;                     (cons (first a-seq) nil))]
; Why this not work T__T
  (if (= in 0)
      '()
      (cons a-seq 
            (rotations-helper (dec in) 
                              (concat (rest a-seq)
                                      (cons (first a-seq) nil))))))

(defn rotations
 "Palauttaa kaikki sekvenssin rotaatiot"
  [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-helper (count a-seq) a-seq)))

(defn count-elem-helper [n elem coll]
    (if (empty? coll)
          n
          (let [new-count (if (= elem (first coll))
                             (inc n)
                             n)]
            (count-elem-helper new-count
                               elem
                               (rest coll)))))

(defn count-elem [elem coll]
     (count-elem-helper 0 elem coll))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
        freqs
        (assoc freqs (first a-seq) (count-elem (first a-seq) a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  [:-])

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

