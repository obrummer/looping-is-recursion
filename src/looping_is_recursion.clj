(ns looping-is-recursion)

(defn power [n exp]
  (let [helper (fn [acc n exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc n) n (dec exp))))]
  (helper 1 n exp)))

(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0

(defn last-element [a-seq]
   (let [helper (fn [acc a-seq]
                 (if (empty? a-seq)
                   acc
                   (recur (first a-seq) (rest a-seq))))]
  (helper nil a-seq)))

(last-element [])      ;=> nil
(last-element [1 2 3]) ;=> 3
(last-element [2 5])   ;=> 5

(defn seq= [seq1 seq2]
   (let [helper (fn [seq1 seq2]
                 (cond 
                   (and (empty? seq1) (empty? seq2))  true
                   (and (not (empty? seq1)) (empty? seq2)) false
                   (and (empty? seq1) (not (empty? seq2))) false
                   (not= (first seq1) (first seq2)) false
                  :else (recur (rest seq1) (rest seq2))))
                   ]
  (helper seq1 seq2)))

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] []) 

(defn find-first-index [pred a-seq]
 (loop [index 0
         pred pred
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) index
      :else (recur (inc index) pred (rest a-seq)))))

(find-first-index zero? [1 1 1 0 3 7 0 2])                    ;=> 3
(find-first-index zero? [1 1 3 7 2])                          ;=> nil
(find-first-index (fn [n] (= n 6)) [:cat :dog :six :blorg 6]) ;=> 4
(find-first-index nil? [])                                    ;=> nil

(defn avg [a-seq]
  (loop [index 0
         sum 0
         a-seq a-seq]
    (cond
      (empty? a-seq) (/ sum index)
      :else (recur (inc index) (+ sum (first a-seq)) (rest a-seq)))))

(avg [1 2 3])   ;=> 2
(avg [0 0 0 4]) ;=> 1
(avg [1 0 0 1]) ;=> 1/2 ;; or 0.5

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [result (set '())
         a-seq a-seq]
    (cond
      (empty? a-seq) result
      :else (recur (toggle result (first a-seq)) (rest a-seq)))))

(parity [:a :b :c])           ;=> #{:a :b :c}
(parity [:a :b :c :a])        ;=> #{:b :c}
(parity [1 1 2 1 2 3 1 2 3 4]) ;=> #{2 4}

(defn fast-fibo [n]
 (loop [Fn-1 0
         Fn 1
         n n]
    (if
        (<= n 0) Fn-1
        (recur Fn (+ Fn-1 Fn) (dec n)))))
(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    (== n 2) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(fast-fibo 0) ;=> 0
(fast-fibo 1) ;=> 1
(fast-fibo 2) ;=> 1
(fast-fibo 3) ;=> 2
(fast-fibo 4) ;=> 3
(fast-fibo 5) ;=> 5
(fast-fibo 6) ;=> 8

(defn cut-at-repetition [a-seq]
  [":("])

