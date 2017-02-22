(ns recursion)


(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))


(defn singleton? [coll]
  (let [x (vec coll)]
    (cond
      (empty? x) false
      (empty? (drop 1 x)) true
      :else false)))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))


(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
    (let [x (count seq-1)
          xx (count seq-2)]
            (if (> x xx) seq-1 seq-2)))

 (defn longest-sequence [a-seq]
   (let [aa (count a-seq)
         bb (first a-seq)
         cc (second a-seq)
         dd (seq-max bb cc)
         ff (filter (fn [x](> (count x) 0)) a-seq)
         hh (fn e [seq-x]
              (if (= 2 (count seq-x))
                (seq-max (first seq-x) (second seq-x))
                (e (assoc (into [] (rest seq-x)) 0 (seq-max (first seq-x) (second seq-x))))))]
                  (cond
                    (= 0 aa) nil
                    (= 1 aa) bb
                    (= 2 aa) dd
                    (< 2 aa) (hh a-seq))))

 (defn my-filter [pred? a-seq]
    (let [x (first a-seq)
          xx (rest a-seq)
          xxx (fn [p? s]
                  (if (p? s)
                    s
                    ))]

            (if (empty? a-seq)
              a-seq
              (if (pred? x)
                (cons (xxx pred? x) (my-filter pred? xx))
                (my-filter pred? xx)))))


(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
      false
      (if (= elem (first a-seq))
        true
        (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (let [newlist ()
        megafun (fn mega [seqway list-of-succesfuls]
                    (if (pred? (first seqway))
                       (mega (rest seqway) (conj list-of-succesfuls (first seqway)))
                       (reverse list-of-succesfuls)))]
                         (if (empty? a-seq)
                           ()
                           (megafun a-seq newlist))))


(defn my-drop-while [pred? a-seq]
  (let [x (my-take-while pred? a-seq)
        y (count x)]
          (drop y a-seq)))


(defn seq= [a-seq b-seq]
  (= a-seq b-seq))

(defn my-map [f seq-1 seq-2]
  (let [newlist ()
         xl (fn xxl [fun seqa seqb lista]
              (if (or (empty? seqa) (empty? seqb))
                (reverse lista)
                (xxl fun (rest seqa) (rest seqb) (conj lista (fun (first seqa) (first seqb))))))]
                  (xl f seq-1 seq-2 newlist)))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (let [a how-many-times
        b what-to-repeat
        g ()
        c (fn d [e f h] (cond
                          (< e 0) ()
                          (= e 0) h
                          :else (d (dec e) b (conj h b))))]
                              (c a b g)))

(defn my-range [up-to]
  (let [a up-to]
    (if (= a 0)
      ()
      (cons (dec a) (my-range (dec a))))))

(defn tails [a-seq]
   (let [a  a-seq
         b  (count a-seq)
         c  0
         d  ()
         e  (fn fun [f g h i] (cond
                              (empty? f) 0
                              (= g h) i
                              :else (fun f g (inc h) (cons (drop h f) i))))]
     (if (empty? a-seq)
       [a-seq]
       (reverse (cons () (e a b c d))))))



(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))


(defn rotations [a-seq]
    (let [a  a-seq
         b  (count a-seq)
         c  0
         d  ()
         e  (fn fun [f g h i] (cond
                              (empty? f) (into () (vector ()))
                              (=  g h) i
                              :else (fun (cons (last f) (take (dec g) f)) g (inc h) (cons (cons (last f) (take (dec g) f)) i))))]
                                 (e a b c d)))


(defn my-frequencies-helper [freqs a-seq]
  (let [a freqs
        b (into () (set a-seq))
        c (count b)
        z (fn rcrs [counteri iteraattori seqway erilaiset] (if (= counteri iteraattori)
                                            seqway
                                            (rcrs counteri (inc iteraattori) (assoc seqway (nth erilaiset iteraattori) 0) erilaiset)))
        x (z c 0 a b)
        y (fn uudestaan [counteri iteraattori seqway erilaiset mappi] (if (= counteri iteraattori)
                                                            mappi
                                                            (uudestaan counteri (inc iteraattori) seqway erilaiset
                                                                       (assoc mappi (nth erilaiset iteraattori)
                                                                         (count
                                                                           (filter
                                                                             (fn [x] (= x (nth erilaiset iteraattori))) a-seq))))))
        d   (y c 0 a-seq b x)]
          d))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [a (keys a-map)
        b (vals a-map)
        d (count a)
        e ()
        c (fn rpt [counteri iteraattori lista valus kees] (if (= counteri iteraattori)
                                            lista
                                            (rpt counteri (inc iteraattori) (conj lista (repeat (nth valus iteraattori) (nth kees iteraattori))) valus kees)))
        f (flatten  (c d 0 e b a))
        ]
    f))

(defn my-take [n coll]
  (let [a (count coll)
        c (- a n)
        b (drop-last c coll)
       ] b))

(defn my-drop [n coll]
  (let [a (count coll)
        c (- a n)
        b (if (<= c 0)
            ()
            (take-last c coll))
         ]b))

(defn halve [a-seq]
  (let [a (int (/ (count a-seq) 2))
        b (my-take a a-seq)
        c (my-drop a a-seq)
        d (vector b c)
        ]
          d))

(defn seq-merge [a-seq b-seq]
  (let [ e (concat a-seq b-seq)
         a (count e)
         c (fn again [counteri iteraattori lista seqway] (cond
                                                           (= counteri iteraattori) lista
                                                           :else (again counteri
                                                                        (inc iteraattori)
                                                                        (conj lista (apply min seqway))
                                                                        (concat
                                                                          (subvec (vec seqway) 0
                                                                                  (.indexOf seqway
                                                                                            (apply min seqway)))
                                                                          (subvec (vec seqway)
                                                                                  (inc
                                                                                    (.indexOf seqway (apply min seqway))))))))]
                                                                                        (reverse (c a 0 () e))))


(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) a-seq
    (singleton? a-seq) a-seq
    :else (seq-merge
            (merge-sort (first (halve a-seq)))
            (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

