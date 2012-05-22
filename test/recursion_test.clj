(ns recursion-test
  (:use recursion
        midje.sweet))

(facts "product"
  (product [])        => 1
  (product [1 2 3])   => 6
  (product [1 2 3 4]) => 24
  (product [0 1 2])   => 0
  (product #{2 3 4})  => 24)

(facts "last-element"
  (last-element [])      => nil
  (last-element [1 2 3]) => 3
  (last-element [2 5])   => 5)

(facts "sequence-contains?"
  (sequence-contains? 3 [1 2 3]) => true
  (sequence-contains? 3 [4 7 9]) => false
  (sequence-contains? :pony [])  => false)

(facts "seq="
  (seq= [1 2 4] '(1 2 4))  => true
  (seq= [1 2 3] [1 2 3 4]) => false
  (seq= [1 3 5] [])        => false)

(facts "power"
  (power 2 2)  => 4
  (power 5 3)  => 125
  (power 7 0)  => 1
  (power 0 10) => 0)

(facts "fib"
  (fib 0) => 0
  (fib 1) => 1
  (fib 2) => 1
  (fib 3) => 2
  (fib 4) => 3
  (fib 5) => 5
  (fib 6) => 8
  (fib 10) => 55)

(facts "my-range"
  (my-range 0)  => nil
  (my-range 1)  => [0]
  (my-range 2)  => [1 0]
  (my-range 3)  => [2 1 0])

(facts "map-1"
  (map-1 identity [])                 => empty?
  (map-1 identity [1 2 3])            => [1 2 3]
  (map-1 count ["aaa" "bb" "cccc"])   => [3 2 4]
  (map-1 first [[1 2] [4] [7 12 28]]) => [1 4 7]
  (map-1 zero? [0 2 0 13 4 0])
  => [true false true false false true])

(facts "tails"
  (tails [1 2 3 4])   => (just [[1 2 3 4] [2 3 4] [3 4] [4] empty?] :in-any-order)
  (tails [])          => (just [empty?] :in-any-order)
  (tails [1])         => (just [[1] empty?] :in-any-order))

(facts "inits"
  (inits [1 2 3 4])   => (just [empty? [1] [1 2] [1 2 3] [1 2 3 4]] :in-any-order)
  (inits [])          => (just [empty?] :in-any-order)
  (inits [1])         => (just [empty? [1]] :in-any-order))

(facts "split-into-monotonics"
  (split-into-monotonics [0 1 2 1 0])   => '((0 1 2) (1 0))
  (split-into-monotonics [0 5 4 7 1 3]) => '((0 5) (4 7) (1 3)))

(facts "rotations"
  (rotations [])      => empty?
  (rotations [1 2 3]) => (just [[1 2 3] [2 3 1] [3 1 2]] :in-any-order)
  (rotations [:a :b]) => (just [[:a :b] [:b :a]] :in-any-order)
  (rotations [1 5 9 2]) => (just '(1 5 9 2) '(2 1 5 9)
                                 '(9 2 1 5) '(5 9 2 1)
                                 :in-any-order)
  (count (rotations [6 5 8 9 2])) => 5)

(facts "my-frequencies"
  (my-frequencies []) => {}
  (my-frequencies [1 1 2 2 :D :D :D]) => {1 2, 2 2, :D 3}
  (my-frequencies [:a "moi" :a "moi" "moi" :a 1])
  => {:a 3, "moi" 3, 1 1})

(facts "un-frequencies"
  (un-frequencies {:a 3 :b 2 "^_^" 1})
  => (just [:a :a :a "^_^" :b :b] :in-any-order)
  (un-frequencies (my-frequencies [:a :b :c :a]))
  => (just [:a :a :b :c] :in-any-order)
  (my-frequencies (un-frequencies {:a 100 :b 10}))
  => {:a 100 :b 10})

(facts "seq-merge"
  (seq-merge [4] [1 2 6 7])        => '(1 2 4 6 7)
  (seq-merge [1 5 7 9] [2 2 8 10]) => '(1 2 2 5 7 8 9 10))

(facts "mergesort"
  (mergesort [])                 => empty?
  (mergesort [1 2 3])            => '(1 2 3)
  (mergesort [5 3 4 17 2 100 1]) => '(1 2 3 4 5 17 100))

(facts "permutations"
  (permutations [])      => empty?
  (permutations [1 5 3])
  => (just [[1 5 3] [5 1 3] [5 3 1] [1 3 5] [3 1 5] [3 5 1]]
           :in-any-order))

(facts "powerset"
  (powerset [])      => '(())
  (powerset [1 2 4]) => (just [empty? [4] [2] [2 4] [1] [1 4] [1 2] [1 2 4]] :in-any-order))
