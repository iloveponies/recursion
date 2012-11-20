(ns recursion)

; Harjoitus 1
; OK, tosin en ole varma onko toi mun ratkaisu se mit� haettiin
; mutta ainakin se tekee sen mit� pyydettiin ja teht�v�ss� ei
; k�sketty k�ytt�� mit��n tietty� menetelm��
(defn product
        [coll]
        (if (empty? coll)
                1
                (apply * coll)
        )
)

; Harjoitus 2
; OK
; Funktio product tsekkaa ensin onko kokoelma tyhj�. Jos on,
; niin se palauttaa arvon 1. Muussa tapauksessa se laskee tuloksen kertomalla
; kokoelman luvut kesken��n ja palauttaa niiden tuloksen.

; Harjoitus 3
; OK
(defn singleton?
        [coll]
        (if (and (empty? (rest coll))
                (not (empty? coll))
                )
                true
                false
        )
)

; Harjoitus 4
; OK? K�ytin last-funktiota
(defn my-last
        [coll]
        (last coll)
)

; Harjoitus 5
; OK
(defn max-element
        [a-seq]
        (if (empty? a-seq)
                nil
                (apply max a-seq)
        )
)

; Harjoitus 6
; OK
(defn seq-max
        [seq-1 seq-2]
        (if (> (count seq-1) (count seq-2))
                seq-1
                seq-2
        )
)



(defn longest-sequence [a-seq]
  [:-])

(defn my-filter [pred? a-seq]
  [:-])

(defn sequence-contains? [elem a-seq]
  :-)

(defn my-take-while [pred? a-seq]
  [:-])

(defn my-drop-while [pred? a-seq]
  [:-])

(defn seq= [a-seq b-seq]
  :-)

(defn my-map [f seq-1 seq-2]
  [:-])

(defn power [n k]
  :-)

(defn fib [n]
  :-)

(defn my-repeat [how-many-times what-to-repeat]
  [:-])

(defn my-range [up-to]
  [:-])

(defn tails [a-seq]
  [:-])

(defn inits [a-seq]
  [:-])

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

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

