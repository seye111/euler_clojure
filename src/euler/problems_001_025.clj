(ns euler.problems-001-025
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (use [euler.lib :only [lazy-fibs
                         prime-factors
                         prime-factors-in-decreasing-order
                         exp
                         factor?
                         primes-less-than
                         lazy-primes]]))

;;;;; 001 ;;;;;
(defn euler-001-a []  
  (reduce + 
    (filter 
      #(zero? (* (rem % 3) (rem % 5))) 
      (range 1000))))

; optimization suggested by Sunny C
(defn euler-001-b []
  (letfn [(sum-mults [d n]
            ; number, c, of multiples of d under n
            (let [c (int (/ (- n 1) d))]
              ; sum of those multiples
              ; (d times cth triange number)
              (* d (/ (* c (+ c 1)) 2))))]
    (-
      (+ 
        (sum-mults 3 1000)
        (sum-mults 5 1000))
      (sum-mults 15 1000))))

; 233168 
(time (euler-001-a))
(time (euler-001-b))



;;;;; 002 ;;;;;
(defn euler-002 [] 
  (reduce + 
    (take-while #(< % 4e6) 
      (filter 
        even? 
        (map 
          first 
          (lazy-fibs [1 2]))))))

; 4613732 
(time (euler-002))



;;;;; 003 ;;;;;
(defn euler-003-a []
  (apply max 
    (prime-factors 600851475143)))

(defn euler-003-b []
 (first (prime-factors-in-decreasing-order 600851475143)))


; 6857
(time (euler-003-a))
(time (euler-003-b))



;;;;; 004 ;;;;;
(defn euler-004 []
  (apply max
    (letfn [(palin? [n] 
              (= (str n)(apply str (reverse (str n)))))]
      (for [x (range 1000 99 -1)
            y (range 1000 99 -1) 
              :when (palin? (* x y))]
        (* x y)))))

; 906609
(time (euler-004))



;;;;; 005 ;;;;;
(defn euler-005 []
  (reduce * 
    (map 
      #(exp (key %)(val %))
      (seq 
        (apply 
          merge-with 
          max 
          (map #(frequencies (prime-factors %)) (range 21)))))))

; 232792560
(time (euler-005))



;;;;; 006 ;;;;;
(defn euler-006 []
    (let [sum (reduce + (range 1 101))
         	sum-of-squares (reduce + (map #(* % %) (range 1 101)))]
      (- 
        (* sum sum) 
        sum-of-squares)))

; 25164150
(time (euler-006))



;;;;; 007 ;;;;;
(defn euler-007 [] 
  (first 
    (drop 
      10000 
      (lazy-primes))))

; 104743
(time (euler-007))



;;;;; 008 ;;;;;
(defn euler-008	[]  
  (with-open [rdr (io/reader "data/008.dat")]
    (letfn [(sum-numeric-chars [s] 
        (reduce 
          * 
          (map #(- (int %) 48) s)))]			
      (apply max
        (map 
           sum-numeric-chars 
          (partition 5 1 
            (apply str (line-seq rdr))))))))

; 40824
(time (euler-008))



;;;;; 009 ;;;;;
(defn euler-009 []
  (first
    (for [a (range 1 999) 
          b (range (inc a) (- 1000 a))
          c [(- 1000 a b)]
            :when (= 
                    (+ (* a a) (* b b)) 
                    (* c c))]  
      (* a b c))))
          
; 31875000
(time (euler-009))



;;;;; 010 ;;;;;
(defn euler-010 []
  (reduce + (primes-less-than 2e6)))

; 142913828922
(time (euler-010))



;;;;; 011 ;;;;;
(defn euler-011 []
  (let [filename "data/011.dat"
        grid (with-open [rdr (io/reader filename)]
                 (vec 
                   (map 
                     #(vec (map (fn [s] (Integer/parseInt s)) (str/split % #"\s"))) 
                     (line-seq rdr))))]
    (letfn [(row [r] (grid r))
            (cell [r c] ((row r) c))
            (column [c] (map #(% c) grid))
            (columns [] (for [c (range 20)] (column c)))
            
            (max-row-prod [row] (apply max (map #(reduce * %) (partition 4 1 row))))
            (horizontal-max [] (apply max (map max-row-prod grid)))
            
            (max-column-prod [col] (apply max (map #(reduce * %) (partition 4 1 col))))
            (vertical-max [] (apply max (map max-column-prod (columns))))
            
            (d-r-diag-seq [r c] (lazy-seq (cons (cell r c) (d-r-diag-seq (inc r) (inc c)))))
            (d-r-chunk-prod [r c] (reduce * (take 4 (d-r-diag-seq r c))))
            (max-d-r-prod [] (apply max
                (for [r (range 17)
                      c (range 17)]
                  (d-r-chunk-prod r c ))))
            
            (d-l-diag-seq [r c] (lazy-seq (cons (cell r c) (d-l-diag-seq (inc r) (dec c)))))
            (d-l-chunk-prod [r c] (reduce * (take 4 (d-l-diag-seq r c))))
            (max-d-l-prod [] (apply max
                (for [r (range 17)
                      c (range 3 20)]
                  (d-l-chunk-prod r c ))))]
      
      (max (horizontal-max) (vertical-max) (max-d-r-prod) (max-d-l-prod)))))

; 76576500
(time (euler-011))



;;;;; 012 ;;;;;
(defn euler-012 []
  (letfn [(triangle-nums [] 
            (map last (iterate (fn [[a b]] [(inc a) (+ b (inc a))]) [1 1])))
              (count-factors [n]
                (let [sqrt (Math/sqrt n)]
                  (-
                    ; pairs of factors either side of square root
                    (* 2 (count (filter #(factor? n %) (range 1 (inc (int sqrt))))))
                    ; if square number, dec factors so sqrt not counted twice
                    (if (= sqrt (int sqrt))
                      1
                      0))))]
 (first (filter #(> (count-factors %) 500) (triangle-nums)))))

; 76576500
(time (euler-012))



;;;;; 013 ;;;;;

(defn euler-013	[]
  (read-string
    (apply 
      str 
      (take 10 
        (str  
          (with-open [rdr (io/reader "data/013.dat")]
            (reduce + 
              (map 
                read-string 
                (line-seq rdr)))))))))

; 5537376230
(time (euler-013))
