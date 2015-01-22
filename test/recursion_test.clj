(ns iloveponies.tests.recursion
  (:refer-clojure :exclude [empty?])
  (:use recursion
        midje.sweet))

(defn empty? [x]
  (and (sequential? x)
       (clojure.core/empty? x)))

(facts "product" {:exercise 1
                  :points 1}
  (product [])        => 1
  (product [1 2 3])   => 6
  (product [1 2 3 4]) => 24
  (product [0 1 2])   => 0
  (product #{2 3 4})  => 24)

(facts "singleton?" {:exercise 3
                     :points 1}
  (singleton? [1])     => true
  (singleton? #{2})    => true
  (singleton? [nil])   => true
  (singleton? [1 nil]) => false
  (singleton? [])      => false
  (singleton? [1 2 3]) => false)

(facts "my-last" {:exercise 4
                  :points 1}
  (my-last [])      => nil
  (my-last [1 2 3]) => 3
  (my-last [2 5])   => 5)

(facts "max-element" {:exercise 5
                      :points 1}
  (max-element [2 4 1 4]) => 4
  (max-element [2])       => 2
  (max-element [])        => nil)

(facts "seq-max" {:exercise 6
                  :points 1}
  (seq-max [1] [1 2])       => [1 2]
  (seq-max [1 2 3] [:a :b]) => [1 2 3]
  (seq-max [1 2] [3 4])     => [3 4])

(facts "longest-sequence" {:exercise 7
                           :points 1}
  (longest-sequence [[1 2] [] [1 2 3]]) => [1 2 3]
  (longest-sequence [[1 2]])            => [1 2]
  (longest-sequence [])                 => nil)

(facts "my-filter" {:exercise 8
                    :points 1}
  (my-filter odd? [1 2 3 4]) => (just '(1 3))
  (my-filter false? [1 2 3]) => empty?
  (my-filter nil? [1 nil 2]) => (just [nil])
  (my-filter (fn [x] (> x 9000)) [12 49 90 9001]) => (just '(9001))
  (my-filter even? [1 3 5 7]) => empty?)

(facts "sequence-contains?" {:exercise 9
                             :points 1}
  (sequence-contains? 3 [1 2 3]) => true
  (sequence-contains? 3 [4 7 9]) => false
  (sequence-contains? :pony [])  => false)

(facts "my-take-while" {:exercise 10
                        :points 1}
  (my-take-while odd? [1 2 3 4])  => '(1)
  (my-take-while odd? [1 3 4 5])  => '(1 3)
  (my-take-while even? [1 3 4 5]) => empty?
  (my-take-while odd? [])         => empty?)

(facts "my-drop-while" {:exercise 11
                        :points 1}
  (my-drop-while odd? [1 2 3 4])  => '(2 3 4)
  (my-drop-while odd? [1 3 4 5])  => '(4 5)
  (my-drop-while even? [1 3 4 5]) => '(1 3 4 5)
  (my-drop-while odd? [])         => empty?)

(facts "seq=" {:exercise 12
               :points 1}
  (seq= [1 2 4] '(1 2 4))  => true
  (seq= [] [])             => true
  (seq= [1 2 nil] [1 2])   => false
  (seq= [1 4 2] [1 2 4])   => false
  (seq= [1 2 3] [1 2 3 4]) => false
  (seq= [1 3 5] [])        => false)

(facts "my-map" {:exercise 13
                 :points 1}
  (my-map + [1 2 3] [4 4 4]) => '(5 6 7)
  (my-map + [1 2 3 4] [0 0 0]) => '(1 2 3)
  (my-map + [1 2 3] []) => empty?)

(facts "power" {:exercise 14
                :points 1}
  (power 2 2)  => 4
  (power 5 3)  => 125
  (power 7 0)  => 1
  (power 0 10) => 0)

(facts "fib" {:exercise 15
              :points 1}
  (fib 0) => 0
  (fib 1) => 1
  (fib 2) => 1
  (fib 3) => 2
  (fib 4) => 3
  (fib 5) => 5
  (fib 6) => 8
  (fib 10) => 55)

(facts "my-repeat" {:exercise 16
                    :points 1}
  (my-repeat 2 :a)    => '(:a :a)
  (my-repeat 3 "lol") => '("lol" "lol" "lol")
  (my-repeat -1 :a)   => empty?)

(facts "my-range" {:exercise 17
                   :points 1}
  (my-range 0)  => empty?
  (my-range 1)  => '(0)
  (my-range 2)  => '(1 0)
  (my-range 3)  => '(2 1 0))

(facts {:exercise 18
        :points 1}
  (facts "tails" 
    (tails [1 2 3 4])   => (just [[1 2 3 4] [2 3 4] [3 4] [4] []] :in-any-order)
    (tails [])          => (just [[]])
    (tails [1])         => (just [[1] []] :in-any-order))

  (facts "inits"
    (inits [1 2 3 4])   => (just [[] [1] [1 2] [1 2 3] [1 2 3 4]] :in-any-order)
    (inits [])          => (just [[]])
    (inits [1])         => (just [[] [1]] :in-any-order)))

(facts "rotations" {:exercise 19
                    :points 1}
  (rotations [])      => (just '(()))
  (rotations [1 2 3]) => (just [[1 2 3] [2 3 1] [3 1 2]] :in-any-order)
  (rotations [:a :b]) => (just [[:a :b] [:b :a]] :in-any-order)
  (rotations [1 5 9 2]) => (just '(1 5 9 2) '(2 1 5 9)
                                 '(9 2 1 5) '(5 9 2 1)
                                 :in-any-order)
  (count (rotations [6 5 8 9 2])) => 5)

(facts "my-frequencies" {:exercise 20
                         :points 1}
  (my-frequencies []) => {}
  (my-frequencies [1 1 2 2 :D :D :D]) => {1 2, 2 2, :D 3}
  (my-frequencies [:a "moi" :a "moi" "moi" :a 1])
  => {:a 3, "moi" 3, 1 1})

(facts "un-frequencies" {:exercise 21
                         :points 1}
  (un-frequencies {:a 3 :b 2 "^_^" 1})
  => (just [:a :a :a "^_^" :b :b] :in-any-order)
  (un-frequencies (my-frequencies [:a :b :c :a]))
  => (just [:a :a :b :c] :in-any-order)
  (my-frequencies (un-frequencies {:a 100 :b 10}))
  => {:a 100 :b 10})

(facts "my-take" {:exercise 22
                  :points 1}
  (my-take 2 [1 2 3 4]) => '(1 2)
  (my-take 4 [:a :b]) => '(:a :b))

(facts "my-drop" {:exercise 23
                  :points 1}
  (my-drop 2 [1 2 3 4]) => '(3 4)
  (my-drop 4 [:a :b]) => empty?)

(facts "halve" {:exercise 24
                :points 1}
  (halve [1 2 3 4])   => ['(1 2) '(3 4)]
  (halve [1 2 3 4 5]) => ['(1 2) '(3 4 5)]
  (halve [1])         => (just empty? '(1)))

(facts "seq-merge" {:exercise 25
                    :points 1}
  (seq-merge [4] [1 2 6 7])        => '(1 2 4 6 7)
  (seq-merge [1 5 7 9] [2 2 8 10]) => '(1 2 2 5 7 8 9 10))

(facts "merge-sort" {:exercise 26
                     :points 1}
  (merge-sort [])                 => empty?
  (merge-sort [1 2 3])            => '(1 2 3)
  (merge-sort [5 3 4 17 2 100 1]) => '(1 2 3 4 5 17 100))

(facts "split-into-monotonics" {:exercise 27
                                :points 2}
  (split-into-monotonics [0 1 2 1 0])   => '((0 1 2) (1 0))
  (split-into-monotonics [0 5 4 7 1 3]) => '((0 5) (4 7) (1 3)))

(facts "permutations" {:exercise 28
                       :points 3}
  (permutations []) => (just empty?)
  (count (permutations (range 5))) => 120
  (permutations [1 5 3])
  => (just [1 5 3] [5 1 3] [5 3 1] [1 3 5] [3 1 5] [3 5 1]
           :in-any-order))

(facts "powerset" {:exercise 29
                   :points 3}
  (powerset [])      => (some-checker (just empty?) (just #{#{}}))
  (powerset [1 2 4]) => (some-checker
                         (just empty?
                               (just 4 :in-any-order)
                               (just 2 :in-any-order)
                               (just 2 4 :in-any-order)
                               (just 1 :in-any-order)
                               (just 1 4 :in-any-order)
                               (just 1 2 :in-any-order)
                               (just 1 2 4 :in-any-order) :in-any-order)
                         (just #{#{} #{4} #{2} #{2 4} #{1} #{1 4} #{1 2} #{1 2 4}}))
  (count (powerset (range 10))) => 1024)
