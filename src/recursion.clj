(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)
         (count seq-2))
    seq-1 seq-2))


(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      ())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and(empty? a-seq)
       (empty? b-seq)) true
   (or (empty? a-seq)
       (empty? b-seq)) false
   (= (first a-seq)
      (first b-seq))    (seq= (rest a-seq)
                              (rest b-seq))
   :else false))


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1)
          (empty? seq-2))
    ()
    (cons (f (first seq-1)
             (first seq-2))
          (my-map f
                  (rest seq-1)
                  (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat
          (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    ()
    (cons (- up-to 1) (my-range (- up-to 1)))))


(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () ())
    (cons (sequence a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [tls (tails a-seq)
        ins (inits a-seq)
        zip (fn [x y] (concat x y))
        ]
    (set (map zip tls ins))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs    (if (contains? freqs (first a-seq))
                         (assoc freqs (first a-seq) (+ (get freqs (first a-seq)) 1))
                         (assoc freqs (first a-seq) 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [[k v] (first a-map)]
      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or ( = n 0)
          (empty? coll))
    ()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or ( = n 0)
          (empty? coll))
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))
        fst  (take half a-seq)
        snd  (drop half a-seq)]
    [fst snd]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (let [fa (first a-seq)
               fb (first b-seq)
               ra (rest  a-seq)
               rb (rest  b-seq)]
           (if (< fa fb)
             (cons fa (seq-merge ra b-seq))
             (cons fb (seq-merge a-seq rb))))))

(defn merge-sort [a-seq]
  (cond
   (< (count a-seq) 2) a-seq
   :else (let [[fst snd] (halve a-seq)
               sfst      (merge-sort fst)
               ssnd      (merge-sort snd)]
           (seq-merge sfst ssnd))))


(defn take-incr [a-seq lst acc]
  (cond
   (empty? a-seq)        (reverse (cons lst acc))
   (= lst nil)           (take-incr (rest a-seq) (first a-seq) acc)
   (> (first a-seq) lst) (take-incr (rest a-seq) (first a-seq) (cons lst acc))
   :else (reverse (cons lst acc))))

(defn take-decr [a-seq lst acc]
  (cond
   (empty? a-seq)        (reverse (cons lst acc))
   (= lst nil)           (take-decr (rest a-seq) (first a-seq) acc)
   (< (first a-seq) lst) (take-decr (rest a-seq) (first a-seq) (cons lst acc))
   :else (reverse (cons lst acc))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [incr (take-incr a-seq nil nil)
          decr (take-decr a-seq nil nil)
          ci   (count incr)
          cd   (count decr)]
      (if (> ci cd)
        (cons incr (split-into-monotonics (drop ci a-seq)))
        (cons decr (split-into-monotonics (drop cd a-seq)))))))



(defn add-elem-pos [a-seq elem pos acc]
  (if (= pos -1)
    acc
    (let [tke (take pos a-seq)
          drp (drop pos a-seq)
          sel (cons elem ())
          cnt (concat tke sel drp)]
      (add-elem-pos a-seq elem (- pos 1) (cons cnt acc)))))

(defn all-elem-seqs [elem acc]
  (if (empty? acc)
    ()
    (concat (add-elem-pos (first acc) elem (count (first acc)) ())
            (all-elem-seqs elem (rest acc)))))

(defn permutation-helper [a-set acc]
  (if (empty? a-set)
    acc
    (let [fs   (first a-set)
          rs   (rest  a-set)
          nacc (all-elem-seqs fs acc)]
      (permutation-helper rs nacc))))

(defn permutations [a-set]
  (if (empty? a-set)
    (cons () ())
    (permutation-helper (rest a-set) (cons (cons (first a-set) ()) ()))))

(defn set-iterator [elem sos acc]
  (if (empty? sos)
    acc
    (let [old (first sos)
          new (conj old elem)
          add (conj acc old new)]
      (set-iterator elem (rest sos) add))))

(defn element-iterator [a-set p-set]
  (if (empty? a-set)
    p-set
    (let [fa (first a-set)
          acc #{}
          new-pset (set-iterator fa p-set acc)]
      (element-iterator (rest a-set) new-pset))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [fst #{(first a-set)}
          rst (rest  a-set)
          p-set (conj #{} fst #{})]
      (element-iterator rst p-set))))

