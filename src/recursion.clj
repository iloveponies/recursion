(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))


(defn singleton? [coll]
  (and
   (not (empty? coll))
   (empty? (rest coll))))



;;mitä tässä nyt oikein tapahtuu??
(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))



(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

    

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))


(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))


(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))
 


(odd? (first [1 2 3]))

(my-filter odd? [1 2 3 5]) 

(= :pony (first [:pony 234]))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(sequence-contains? 3 [1 2 3]) ;=> true
(sequence-contains? 3 [4 7 9]) ;=> false
(sequence-contains? :pony [])  ;=> false

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))
 
     
(my-take-while odd? [1 2 3 4])  ;=> (1)
(my-take-while odd? [1 3 4 5])  ;=> (1 3)
(my-take-while even? [1 3 4 5]) ;=> ()
(my-take-while odd? [])         ;=> ()

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '() 
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (cons (first a-seq) (rest a-seq))))

(my-drop-while odd? [1 2 3 4])
(my-drop-while odd? [1 3 4 5])
(my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
(my-drop-while odd? []) 

(defn seq= [a-seq b-seq]
  (cond
    (and (or (empty? a-seq) (empty? b-seq)) (not (and (empty? a-seq) (empty? b-seq)))) false
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))





     
(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))


(my-map + [1 2 3] [4 4 4])


(my-map + [1 2 3] [1 2 3 4])

(my-map + [1 2] [1 2 3])

(my-map + [] [1 2])

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

    
(defn fib [n]
  (cond
    (zero? n) 0
    (== n 1) 1
    :else (+ (fib (- n 1))
             (fib (- n 2)))))

(fib 20)


(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) 
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))


(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(my-range 0)

(my-range 20)



(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (cons (first a-seq) (rest a-seq)) (tails (rest a-seq))))) 

;;(inits [1 2 3 4 5 6])

(tails [1 2 3 4])

(defn inits [a-seq]
  (let [fun (fn [x] (if (empty? a-seq)
    '(())
    (cons (cons (first a-seq) (rest a-seq)) (inits (reverse (rest (reverse a-seq)))))))]
    (fun a-seq)))

(reverse (inits [1 2 3 4]))


(map concat (tails [1 2 3 4]))

(reverse (inits [1 2 3 4]))

(<= 1 1)


(tails [1 2 3 4])


;; cons tää
(rest (tails [1 2 3 4])) ;=> ((2 3 4), (3 4), (4) ())
;; ja täää
(rest (reverse (inits [1 2 3 4]))) ;=> (1) (1 2) (1 2 3) (1 2 3 4)



(map concat (rest (tails [1 2 3 4])) (rest (reverse (inits [1 2 3 4]))))


(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map concat (rest (tails a-seq)) (rest (reverse (inits a-seq))))))



(rotations [1 2])

(rotations '(1 2 2))



(count (rotations []))

(concat (rest '((4) 1 2 3)))

(rotations [1 2 3])

(rotations [1 2 3 4])   
                                        ;

  ;;(if (rotations
                                        ; tails + inits !! cons ! oikea järj.

;; (1) (1 2) (1 2 3) (1 2 3 4)
;; (
                           






;(defn my-frequencies-h(tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())
;(inits [1 2 3 4elper [freqs a-seq]
;  [:-])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn count-elem [elem coll]
    (count-elem-helper 0 elem coll))


(count-elem "moi" ["mou" "moi" "moo"])

;(defn my-frequencies-helper [freqs a-seq]
;  (if (empty? a-seq)
;    blah
;    (count-elem freqs a-seq)))



;(defn my-frequencies-helper [freqs a-seq]
;  (if (freqs 


 

(defn my-frequencies [a-seq]
  (if (empty? a-seq)
    {}
    (let [freqs (map count-elem a-seq (repeat (count a-seq) a-seq))
          create-map (fn [a b] (assoc {} a b))]
      (apply merge (map create-map a-seq freqs)))))
  
(my-frequencies [:a "moi" :a "moi" "moi" :a 1])
  
(my-frequencies [1 2 3 3 3 3 1 2 3 "foo" "fuu"])

(my-frequencies [])
  
(keys (my-frequencies ["moi" "moi" "moi" "moi" "muu" "muu"]))

(map repeat '(2 4) '("moi" "muu"))

(defn un-frequencies [a-map]
  (let [freqs (vals a-map)
        jutut (keys a-map)]
    (apply concat (map repeat freqs jutut))))


(map repeat '(3 4 5) '("foo" "fii" "faa"))

(my-frequencies ["foo" "fuu" "foo"])




(defn my-take-helper [a b c]
  (if (or (>= a b) (empty? c))
    '()
    (cons (first c) (my-take-helper (inc a) b (rest c)))))
                
(defn my-take [n coll]
  (my-take-helper 0 n coll))

    
(my-take 2 ["foo" "fuu" "faa" "fii"])

(my-take 6 '(1 2 3 4))

(rest '())

(defn my-drop-helper [a b c]
  (if (or (empty? c) (>= a b))
    c
    (my-drop-helper (inc a) b (rest c))))

(defn my-drop [n coll]
  (my-drop-helper 0 n coll))





(defn halve [a-seq]
  (let [size (count a-seq)
        n (int (/ size 2))]
    (vector (take n a-seq) (drop n a-seq))))

(halve [1 2 3 4 5 6])



(defn seq-merge [a-seq b-seq]
    (cond
      (and (empty? a-seq) (empty? b-seq)) '()
      (empty? a-seq) (cons (first b-seq) (seq-merge [] (rest b-seq)))
      (empty? b-seq) (cons (first a-seq) (seq-merge (rest a-seq) []))
      (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
      (>= (first a-seq) (first b-seq)) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))



(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(merge-sort [8 41 45 3 1 3 2 1 4 75 12 3 4 21])








(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [monotonic? (fn [x] (or (apply <= x) (apply >= x)))
          great-mono (first (reverse (take-while monotonic? (rest (reverse (inits a-seq))))))
          size-of-mono (count great-mono)]
      (cons great-mono (split-into-monotonics (drop size-of-mono a-seq))))))

(split-into-monotonics [0 5 4 7 1 3])
(split-into-monotonics [0 1 2 1 0])







(defn permutations [a-set]
  (let [n (count a-set)
        add-new (fn [x] (conj x (first a-set)))]
    (cond
      (= 0 n) '(())
      (= 1 n) (concat a-set)
      (= 2 n) (cons (concat a-set) (cons (reverse (concat a-set)) '()))
      :else (apply concat (map rotations (map add-new (permutations (rest a-set))))))))


        


;;(clojure.set/union #{#{}} #{#{1}})



;;(map (fn [x] (conj x 1)) #{#{}}) ;; TÄMÄ ON OIKEA TAPA!


;;(map conj #{#{} #{1}} #{#{2} #{2}})




(+ 1 2)                                        
                                        
(defn powerset [a-set]
  (let [size (count a-set)]
    (cond
      (= size 0) #{#{}}
      :else (let [a (first a-set)
                  b (set (rest a-set))
                  add-atom (fn [x] (conj x a))]
              (clojure.set/union (map add-atom (powerset b)) (powerset b))))))


(count (powerset #{1 2 3 4 5 6}))

