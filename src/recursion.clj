(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  ;(println "sing:" coll)
  ;
  ; c'e' first ma non last
  ; first può essere nil -> vale true
  ; necessito quindi dell'or
  ;
  (if (empty? coll)
    false
    (and (or (first coll) (nil? (first coll)))
         (empty? (rest coll)))))

(defn my-last [coll]
  (if (empty? coll)
    nil ;exit
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil ;exit
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [[c1 c2] [(count seq-1) (count seq-2)]]
    (if (> c1 c2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]; trasforma in vettore, altrimenti ritorna ([..] [..])
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) ; se vero lo aggiungo al risultato
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)) ; se falso, lo scarto e proseguo
      )))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (not= elem (first a-seq))
     (sequence-contains? elem (rest a-seq))
   :else
     true))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     '() ; exit
   (pred? (first a-seq))
     (cons (first a-seq)
           (my-take-while pred? (rest a-seq)))
   :else
     '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     '() ; exit
   (pred? (first a-seq))  ; se vero lo droppo e passo al prossimo
     (my-drop-while pred? (rest a-seq))
   :else ; al primo falso ritorno cio che resta
     a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     true
   (or (empty? a-seq) (empty? b-seq))
     false
   (= (first a-seq) (first b-seq))
     (seq= (rest a-seq) (rest b-seq))
   :else
     false
   ))

(defn my-map [f seq-1 seq-2]
  (cond
   (and (empty? seq-1) (empty? seq-2))
     seq-1
   (or (empty? seq-1) (empty? seq-2))
     []
   :else
     (cons (f (first seq-1) (first seq-2))
           (my-map f (rest seq-1) (rest seq-2))
)))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))
  ))

(defn fib [n]
  (cond
   (<= n 1)
     n
   :else
     (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
  ))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (- up-to 1)))
  ))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations-helper[a-seq n]
  (if (<= n 0)
    ()
    ;; calcolo una rotazione e la chiamo x
    (let [x (concat (rest a-seq) (list (first a-seq)))]
    ;; aggiungo la rotazione alla risposta con chiamata ricorsiva n volte
     (cons x (rotations-helper x (dec n))))
  )
)

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-helper a-seq (count a-seq))))

;(defn my-frequencies-helper_2 [freqs a-seq]
;  (if (empty? a-seq)
;    freqs
;    (let [x (first a-seq)]
;      (if (freqs x) ;;recupera il valore
;        (conj freqs  (my-frequencies-helper (assoc freqs x (inc (freqs x))) (rest a-seq)))
;        (conj freqs (my-frequencies-helper (assoc freqs x 1) (rest a-seq)))
;        )
;  )))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [x (first a-seq)]
      (conj freqs
            (if (freqs x) ;;recupera il valore
              (my-frequencies-helper (assoc freqs x (inc (freqs x))) (rest a-seq))
              (my-frequencies-helper (assoc freqs x 1) (rest a-seq))
            ))))
)

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [x (first a-map)]
      (let [[symb cnt] x]
        (concat (repeat cnt symb) (un-frequencies (rest a-map)))
      )
)))

(defn my-take [n coll]
  (if (or (empty? coll) (== n 0))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))
  )
)

(defn my-drop [n coll]
  (if (or (empty? coll) (== n 0))
    coll
    (my-drop (dec n) (rest coll)))
  )


(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    (vector (my-take n a-seq) (my-drop n a-seq)))
)

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq)
     b-seq
   (empty? b-seq)
     a-seq
   :else
     (let [[x y] [(first a-seq) (first b-seq)]]
       (if (< x y)
          (cons x (seq-merge (rest a-seq) b-seq))
          (cons y (seq-merge a-seq (rest b-seq)))
        )
)))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[a b] (halve a-seq)]
      (concat (seq-merge (merge-sort a) (merge-sort b))))
))

(defn is-monotonic? [a-seq]
  (if (<= (count a-seq) 1)
    true
    (let [[x y] [(first a-seq) (second a-seq)]]
      (if (<= x y)
        (is-monotonic? (rest a-seq))
        false
))))

(defn split-into-monotonics [a-seq]
  (last (take-while is-monotonic? (inits a-seq))))

(defn permutations-helper [a-set]
  (if (empty? a-set)
    ()
    (let [[a brot] [(first a-set) (rotations (rest a-set))]]
      (println "set" a-set)
      (println "a" a,brot)

      (cons a (map permutations-helper brot))
      )
    )
  )

(defn permutations [a-set]
  ;; (1 2 3) -> ((2 3 1) (3 1 2) (1 2 3))
  (permutations-helper (rotations a-set))
)

(defn powerset [a-set]
  [:-])

