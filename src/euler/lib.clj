(ns euler.lib)

(defn lazy-fibs 
  ([] (lazy-fibs [0 1]))
  ([start-vector] (iterate (fn [[ a b]] [b (+ a b)]) start-vector)))

(defn factor? [n d] (zero? (rem n d)))

(defn has-factor-in? [n coll]
  (if (some #(factor? n %) coll)
    true
    false))

(defn next-odd [n]
  (+ n (if (even? n) 1 2)))

(defn lazy-primes [] 
  (letfn [(add-next-prime [primes-so-far]
            (conj 
              primes-so-far
                (loop [candidate (next-odd (last primes-so-far))]
                  (letfn [(elimination-factors []
                        (take-while #(<= % (Math/sqrt candidate)) primes-so-far))
                          (prime? [n] 
                            (not (has-factor-in? n (elimination-factors))))]
                    (if (prime? candidate)
                      candidate
                      (recur (+ candidate 2)))))))]
    (map last (iterate add-next-prime [2]))))

(defn prime-factors [n]
  (loop [quotient n
         primes (lazy-primes)
         result []]
    (let [prime (first primes)]
      (if (< quotient prime)
        result
        (if (factor? quotient prime)
          (recur (/ quotient prime) primes (conj result prime))
          (recur quotient (rest primes) result))))))

(defn exp [a b]
    (reduce * (repeat b a)))

(defn primes-less-than [n]
    (letfn [(drop-multiples [p col]
              (remove #(zero? (rem % p)) col))
            (seive [[p & col :as all]]
              (if (< (* p p) n)
                (cons p (seive (drop-multiples p col)))
                all))]
  (seive (range 2 n))))

