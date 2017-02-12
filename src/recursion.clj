(ns recursion)

(defn product
  "Get the product of given collection, in recursive manner."
  [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton?
  "Find out, if this collection contains only one item in."
  [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last
  "My very special implementation of last."
  [coll]
  (if (empty? coll)
   nil
   (if (singleton? coll) (first coll) (my-last (rest coll)))))

(defn extrema-by-comprasion
  "Gets best element from a sequence, using comprasion by some predicate."
  [a-seq predicate]
  (let [rest-elements (rest a-seq)]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   (singleton? rest-elements) (apply predicate a-seq)
   :else (let [first-elem (first a-seq)
               second-elem (first rest-elements)]
     (if (= first-elem (predicate first-elem second-elem))
         (extrema-by-comprasion
            (cons first-elem (rest rest-elements)) predicate)
         (extrema-by-comprasion rest-elements predicate))))))

(defn max-element
  "Gets max element from a sequence."
  [a-seq]
  (extrema-by-comprasion a-seq max))

(defn seq-max
  "Gets a longer sequence."
  [seq-1 seq-2]
  (let [length-first (count seq-1) length-second (count seq-2)]
    (if (> length-first length-second) seq-1 seq-2)))

(defn longest-sequence
  "Gets a longest sequence from this sequence of sequences."
  [a-seq]
  (extrema-by-comprasion a-seq seq-max))

(defn my-filter
  "My special implementation of filter."
  [pred? a-seq]
  (let [singleton-filter
        (fn
           [sequence]
            (let [first-element (first sequence)]
              (if (pred? first-element) [first-element] [])))]
  (cond (empty? a-seq) a-seq
        (singleton? a-seq) (singleton-filter a-seq)
        :else
        (concat
          (singleton-filter a-seq)
          (my-filter pred? (rest a-seq))))))

(defn sequence-contains?
  "Find out, if this sequence contains given element."
  [elem a-seq]
  (let [equality-predicate (fn [sequence-element] (= sequence-element elem))]
    (cond
      (equality-predicate (first a-seq)) true
      (empty? a-seq) false
      :else (sequence-contains? elem (rest a-seq)))))

(defn my-take-while
  "Returns the longest prefix of a-seq
  where pred? returns true for every element."
  [pred? a-seq]
  (let [first-elem (first a-seq)]
    (if
      (or (empty? a-seq) (not (pred? first-elem)))
      '()
      (cons first-elem (my-take-while pred? (rest a-seq))))))

(defn my-drop-while
  "Drops elements from sequence, until pred? returns false."
  [pred? a-seq]
  (let [first-elem (first a-seq)] (cond
     (empty? a-seq) '()
     (pred? first-elem) (my-drop-while pred? (rest a-seq))
     :else a-seq)))

(defn seq=
  "Compares two sequences for equality."
  [a-seq b-seq]
  (let [a-is-empty (empty? a-seq)
        b-is-empty (empty? b-seq)
        both-empty (and a-is-empty b-is-empty)
        one-empty (or a-is-empty b-is-empty)]
  (cond
    both-empty true
    (and (not one-empty) (= (first a-seq) (first b-seq)))
       (seq= (rest a-seq) (rest b-seq))
    :else false)))

(defn my-map
  "My very special implementation of map!"
  [f seq-1 seq-2]
  (let [first-elem-1-seq (first seq-1) first-elem-2-seq (first seq-2)]
    (if
      (or (= first-elem-1-seq nil) (= first-elem-2-seq nil))
      '()
      (cons
        (f first-elem-1-seq first-elem-2-seq)
        (my-map f (rest seq-1) (rest seq-2))))))

(defn power
  "Power is for cowards."
  [n k]
  (if (zero? k)
   1
   (* n (power n (dec k)))))

(defn fib
  "Fibonacci classics."
  [n]
  (cond
    (zero? n) 0
    (== 1 n) 1
    :else (+ (fib (dec (dec n))) (fib (dec n)))))

(defn my-repeat
  "My implementation of repeat."
  [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
   '()
   (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range
  "My really special implementation of range."
  [up-to]
  (let [dec-up-to (dec up-to)]
    (cond
      (zero? up-to) '()
      :else (conj (my-range dec-up-to) dec-up-to))))

(defn inits
  "Constructs a sequence of prefixal sequences from given sequence."
  [a-seq]
  (let [tailor (fn tailor
              [sequence counter when-to-stop] (if
              (== counter when-to-stop)
              '()
              (cons
                (first sequence)
                (tailor (rest sequence) (inc counter) when-to-stop))))]
        (map (partial tailor a-seq 0) (range 0 (inc (count a-seq))))))

(defn tails
  "Constructs a sequence of suffixal sequences from a given sequence."
  [a-seq]
  (let [num-items (count a-seq)
        suffix (fn suffix [ctr]
                (if
                   (== num-items ctr)
                   '( ())
                   (cons (drop ctr a-seq) (suffix (inc ctr)))))]
                   (suffix 0)))

(defn rotations
  "Constructs rotations of a given sequence."
  [a-seq]
  (if (empty? a-seq)
      '(())
      (rest
        (map
          (fn [vec] (apply concat vec))
          (map vector (tails a-seq) (inits a-seq))))))

(defn my-frequencies-helper
  "A recursive helper function for my frequencies."
  [freqs a-seq]
    (if (empty? a-seq) freqs
      (let [current (first a-seq)
            rest-elements (rest a-seq)]
            (if (contains? freqs current)
              (my-frequencies-helper
                  (assoc
                   freqs
                   current (inc (get freqs current)))
                   rest-elements)
               (my-frequencies-helper (assoc freqs current 1)
               rest-elements)))))

(defn my-frequencies [a-seq]
  "My frequencies implementation."
  (my-frequencies-helper {} a-seq))

(defn un-frequencies
  "Take a map of frequencies, and give out a source array."
  [a-map]
  (let [un-frequencies-helper (fn un-frequencies-helper [inner-map] (if
            (empty? inner-map)
            '()
            (cons
              (apply repeat (reverse (first inner-map)))
              (un-frequencies-helper (rest inner-map)))))]
    (apply concat (un-frequencies-helper a-map))))

(defn my-take
  "Take some items from a start of collection."
  [n coll]
  (let [my-take-helper (fn my-take-helper [ctr sub-seq]
          (if (or (== ctr n) (empty? sub-seq))
              '()
              (cons
                  (first sub-seq)
                  (my-take-helper (inc ctr) (rest sub-seq)))))]
    (my-take-helper 0 coll)))

(defn my-drop
  "Drop some items right from an end of collection."
  [n coll]
  (let [my-drop-helper (fn my-drop-helper [ctr sub-seq] (let
          [rest-sub-seq (rest sub-seq)]
          (if
            (or (empty? sub-seq) (== ctr n)) sub-seq
            (my-drop-helper (inc ctr) rest-sub-seq))))]
            (my-drop-helper 0 coll)))

(defn halve
  "Return two halves of this sequence, the larger comes first."
  [a-seq]
  (let [first-half-num-elements (int (/  (count a-seq) 2))]
  (vector
    (my-take first-half-num-elements a-seq)
    (my-drop first-half-num-elements a-seq))))

(defn seq-merger
  "Return a merger of two sorted sequences into given stash."
  [a-seq b-seq stash]
  (cond
    (empty? a-seq) (concat stash b-seq)
    (empty? b-seq) (concat stash a-seq)
    :else
      (let
        [first-elem-a-seq (first a-seq) first-elem-b-seq (first b-seq)]
        (if
          (< first-elem-a-seq first-elem-b-seq)
          (seq-merger (rest a-seq) b-seq (concat stash [first-elem-a-seq]))
          (seq-merger a-seq (rest b-seq) (concat stash [first-elem-b-seq]))))))

(defn seq-merge
  "Return a merger of two sorted sequences."
  [a-seq b-seq]
  (seq-merger a-seq b-seq '()))

(defn merge-sort
  "A merge sort classics."
  [a-seq]
  (let [num-items (count a-seq)]
   (cond (>= 1 num-items) a-seq
         (== 2 num-items) (let [first-elem (first a-seq)
                                second-elem (first (rest a-seq))]
           (if (< first-elem second-elem)
              [first-elem second-elem]
              [second-elem first-elem]))
          :else
            (apply seq-merge (map merge-sort (halve a-seq))))))

(defn monotonic?
  "Returns true if a-seq is monotonic, false otherwise."
  [a-seq]
  (and (not (empty? a-seq)) (or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics
  "Splits given sequence into monotonics."
  [a-seq]
  (let [largest-monotonic-prefix (last (filter monotonic? (inits a-seq)))
        prefix-length (count largest-monotonic-prefix)]
     (if (empty? a-seq)
         '()
         (cons
           largest-monotonic-prefix
           (split-into-monotonics (drop prefix-length a-seq))))))

(defn remove-item
  "A helper to remove (unique) item from sequence."
  [a-seq item] (remove #(== item %) a-seq))

(def permutations "This is necessary, since indirect recursion happens below!")

(defn permutations-interim
  "A helper, which inherently calls permutations function to smaller a-seq."
  [a-seq item]
  (map #(cons item %) (permutations (remove-item a-seq item))))

(defn permutations
  "Counts all permutations of a given set."
  [a-set]
  (let [a-seq (vec a-set) num-items (count a-seq)]
    (cond (== 0 num-items) [[]]
        (== 1 num-items) a-seq
        (== 2 num-items) (let
          [first-item (get a-seq 0) second-item (get a-seq 1)]
          [[first-item second-item] [second-item first-item]])
        :else  (apply
                concat (map (partial permutations-interim a-seq) a-seq)))))

(def powerset "The same indirect recursion trick.")

(defn powerset-interim
  "A helper, which inherently calls powerset."
  [a-set item]
  (conj (powerset (set (remove-item a-set item))) a-set))

(defn powerset
  "Given a set, returns a powerset of that set."
   [a-seq]
   (let [a-set (set a-seq) num-items (count a-set)]
    (cond (== 0 num-items) #{#{}}
          (== 1 num-items) #{#{} (set a-set)}
          :else (set
                (apply concat (map (partial powerset-interim a-set) a-set))))))
