(ns recursion)

(defn product [coll] (if (empty? coll) 1 (* (first coll) (product (rest coll))))
  )
;(product [1 2 3 4]) ;=> 24

(defn singleton? [coll]  (if (empty? coll) false (empty? (rest coll)))
  )
;(singleton? [1])     ;=> true
;(singleton? #{2})    ;=> true
;(singleton? [])      ;=> false
;(singleton? [1 2 3]) ;=> false

(defn my-last [coll] (if (empty? coll) nil (if (singleton? coll) (first coll) (my-last (rest coll))))
  )
;(my-last [])      ;=> nil
;(my-last [1 2 3]) ;=> 3

(defn max-element [a-seq] (if (empty? a-seq) nil (reduce max a-seq))
  )
(defn max-el [aseq] (let [a1 (first aseq) a2 (rest aseq)]
                      (cond
                        (empty? aseq) nil
                        (empty? a2) a1
                        :else (if (singleton? a2) (max a1 (first a2)) (max a1 (max-el a2))))))

;(max-el [2 4 1 4]) ;=> 4
;(max-el [2])       ;=> 2
;(max-el [])        ;=> nil

(defn seq-max [seq-1 seq-2] (let [s1 (count seq-1) s2 (count seq-2)]
                              (if (> s1 s2) seq-1 seq-2)))
;(seq-max [1] [1 2])   ;=> [1 2]
;(seq-max [1 2] [3 4]) ;=> [3 4]

(defn longest-sequence [aseq] (let [a1 (first aseq) a2 (rest aseq)]
                      (cond
                        (empty? aseq) nil
                        (empty? a2) a1
                        :else (if (singleton? a2) (seq-max a1 (first a2)) (seq-max a1 (longest-sequence a2))))))
;(longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
;(longest-sequence [[1 2]])            ;=> [1 2]
;(longest-sequence [])                 ;=> nil

(defn my-filter [pred? aseq] (let [a1 (first aseq) a2 (rest aseq)]
                                (cond
                        (empty? aseq) ()
                        (pred? a1) (cons a1 (my-filter pred? a2))
                        :else (my-filter pred? a2)))
  )
;(my-filter odd? [1 2 3 4]) ;=> (1 3)
;(my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
;(my-filter even? [1 3 5 7]) ;=> ()

(defn sequence-contains? [elem aseq] (let [a1 (first aseq) a2 (rest aseq)]
                                       (cond 
                                        (empty? aseq) false
                                        (= elem a1) true
                                        :else (sequence-contains? elem a2))))

;(sequence-contains? 3 [1 2 3]) ;=> true
;(sequence-contains? 3 [4 7 9]) ;=> false
;(sequence-contains? :pony [])  ;=> false

(defn my-take-while [pred? aseq] (let [a1 (first aseq) a2 (rest aseq)]
                                   (cond 
                                     (empty? aseq) ()
                                     (pred? a1) (cons a1 (my-take-while pred? a2))
                                     :else ())))
;(my-take-while odd? [1 2 3 4])  ;=> (1)

(defn my-drop-while [pred? aseq] (let [a1 (first aseq) a2 (rest aseq)]
                                   (cond 
                                     (empty? aseq) ()
                                     (pred? a1) (my-drop-while pred? a2)
                                     :else aseq)))
;(my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)

(defn seq= [aseq bseq] (let [a1 (first aseq) a2 (rest aseq) b1 (first bseq) b2 (rest bseq)]
         (cond
           (empty? aseq) (if (empty? bseq) true false)
           (empty? bseq)  false
           (= a1 b1) (seq= a2 b2)
           :else false)))
;(seq= [1 2 4] '(1 2 4))
;(seq= [] '(1 2 4))

(defn my-map [f aseq bseq]  (let [a1 (first aseq) a2 (rest aseq) b1 (first bseq) b2 (rest bseq)]
                        (cond
                          (or (empty? aseq) (empty? bseq)) ()
                          :else (cons (f a1 b1) (my-map f a2 b2)))))

;(my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)

(defn power [n k] (if (zero? k) 1 (* n (power n (dec k))))
  )
;(power 5 3)  ;=> 125

(defn fib [n] (cond
                (zero? n) 0
                (= n 1) 1
                :else (+ (fib (- n 2)) (fib (dec n)))))
;(fib 6) ;=> 8

(defn my-repeat [how-many-times what-to-repeat] (if (pos? how-many-times)
                                                  (cons what-to-repeat (my-repeat (dec how-many-times)  what-to-repeat))
                                                  ()))
;(my-repeat 3 "lol") ;=> ("lol" "lol" "lol")

(defn my-range [up-to] (let [du (dec up-to)] (if (zero? up-to) () (cons du (my-range du)))))
;(my-range 3)  ;=> (2 1 0)

(defn tails ([aseq] (tails aseq []))
            ([aseq res] (let [cnj (conj res aseq)] (if (empty? aseq) cnj (tails (rest aseq) cnj)) )))
;(tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())

(defn inits [a-seq] (reverse (map reverse (tails (reverse a-seq)))))
(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))

(defn rotations ([aseq] (if (empty? aseq) [[]] (rotations aseq [] (count aseq))))
                ([aseq res nm] (let [cnj (conj res aseq)] 
                                 (if (zero? nm) res (rotations  (concat (rest aseq) [(first aseq)]) cnj (dec nm))))))
(rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))

(defn my-frequencies-helper [freqs aseq] (let [a1 (first aseq) na1 (get freqs a1)] 
                                          (cond 
                                            (empty? aseq) freqs
                                            (nil? na1) (my-frequencies-helper (assoc freqs a1 1) (rest aseq))
                                            :else (my-frequencies-helper (assoc freqs a1 (inc na1)) (rest aseq))  )))
(defn my-frequencies [a-seq] (my-frequencies-helper {} a-seq)
  )


;(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}

(defn un-frequencies  ([a-map]  (un-frequencies a-map []))
                      ([a-map res]  (let [a1 (ffirst a-map) a2 (last (first a-map))] 
                                    (cond
                                      (empty? a-map) res
                                      :else (un-frequencies (dissoc a-map a1) (concat (my-repeat a2 a1) res))))))
;(un-frequencies {:a 3 :b 2 "^_^" 1})             ;=> (:a :a :a "^_^" :b :b)

(defn my-take ([n coll] (my-take n [] coll))
              ([n res coll] (if (or (zero? n) (empty? coll))
                              res
                              (my-take (dec n) (conj res (first coll)) (rest coll)))))
;(my-take 2 [1 2 3 4]) ;=> (1 2)

;(sequence-contains? 3 [1 2 3])
(defn my-drop ([n coll]     (my-drop n [] coll))
              ([n res coll] (if (empty? coll)
                              res
                              (if (zero? n)
                                     (my-drop 0 (conj res (first coll)) (rest coll))
                                     (my-drop (dec n) res (rest coll))))))
 
;(my-drop 1 [1 2 3 4]) ;=> (3 4)

(defn halve [a-seq] (let [hf (int (/ (count a-seq) 2))] (vector (my-take hf a-seq) (my-drop hf a-seq))))
;(halve [1 2 3 4])   ;=> [(1 2) (3 4)]

(defn seq-merge ([aseq bseq] (seq-merge aseq bseq []))
                ([aseq bseq res] (let [a1 (first aseq) a2 (rest aseq) b1 (first bseq) b2 (rest bseq)] 
                              (cond
                                (empty? aseq) (concat res bseq)
                                (empty? bseq) (concat res aseq)
                                (< a1 b1) (seq-merge a2 bseq (concat  res (vector a1)))
                                (>= a1 b1) (seq-merge aseq b2 (concat res (vector b1) ))))))
;(seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
;(seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)

(defn merge-sort [a-seq] (let [aseq (halve a-seq) as1 (first aseq) as2 (last aseq)]
                           (cond
                             (< (count as1) 2) (if (< (count as2) 2) (seq-merge as1 as2) (seq-merge as1 (merge-sort as2)))
                             (< (count as2) 2) (seq-merge (merge-sort as1) as2)
                             :else (seq-merge (merge-sort as1) (merge-sort as2)))))
;(merge-sort [5 3 4 17 2 100 1]) ;=> (1 2 3 4 5 17 100)


(defn getC [a b] (if (> a b) > <))
(defn ok [op s] (let [sp (take-last 2 s) ] (apply op sp)))  ;(ok < [445 10 4]) -> false

(defn split-into-monotonics ([a-seq]   (split-into-monotonics a-seq []))
                            ([a-seq r] (let [p (my-drop 2 (inits a-seq)) 
                                             pc (if (empty? p) not= (apply getC (first p)))
                                             un (last (take-while #(ok pc %) p )) ] 
                                         (cond 
                                           (empty? a-seq) r
                                           (singleton? a-seq) (conj r (into () a-seq))
                                           :else (split-into-monotonics (my-drop (count un) a-seq) (conj r un))))))



;(split-into-monotonics [0 1 2 1 0 4 5 8 7])   ;=> ((0 1 2) (1 0))
;(split-into-monotonics [0 5 4 7 1 3]) ;=> ((0 5) (4 7) (1 3))


(defn existuje?[co kde] (boolean (some #(= co %) kde)))
;(existuje? 1 [2 5])
;(map inc #{1 3 5})
(defn doplnEl [cim co] (filter not-empty (map (fn [a b] (if ((complement existuje?) a b) ((comp flatten vector) a b) []))  (repeat cim) co)))
;(doplnEl 5 [[1 2] [5 4] [4 7]] )
(defn dopln [mncim co] (mapcat #(doplnEl % co) mncim))
;(dopln #{1 3 5} [[1 2] [5 4] [4 7]]) 
;(first #{1 5 3})


(defn permutations  ([a-set]      (if (empty? a-set) [[]] (permutations a-set (map vector a-set) (count a-set)))) 
                    ([a-set am n] (cond
                                    (= n 1) am
                                    :else (permutations a-set (dopln a-set am) (dec n)))))
;(count (permutations #{1 5 3 0}))
;(permutations #{1 3 5 6})


(defn bin->sset ([n lst] (bin->sset n (rotations lst) #{}))
                ([n lst v] (cond
                             (zero? n) v
                             (odd? n) (bin->sset (int (/ n 2)) (rest lst)  (conj v (ffirst lst)))
                             :else (bin->sset (int (/ n 2)) (rest lst)  v )))
                             
                             )

(bin->sset 4 [:a :b :c :d :e :f])
(defn powerset [x] (map #(bin->sset % x) (range 0 (Math/pow 2 (count x)))))
(powerset #{1 2 3})

