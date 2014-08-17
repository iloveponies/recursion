(use 'recursion :reload)

(defn my-frequencies2 "count frequencies of different elements"
  ([a-seq] (my-frequencies2 a-seq {}))
  ([a-seq freqs]
   (let [count-item
         (fn [item coll]
           (assoc coll item (inc (get coll item 0))))]
     (if (empty? a-seq)
       freqs
       (recur (next a-seq) (count-item (first a-seq) freqs))))))

(my-frequencies []) ;=> {}
(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}

(def fs {:m 67 :a 0  :c 4 :b 2} )

(into (sorted-map) fs)

(defn count-item [item coll]
  (assoc coll item (inc (get fs item 0))))

(count-item :b fs)

(doc sorted-map)
