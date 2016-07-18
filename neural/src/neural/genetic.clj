(ns neural.genetic
  (:require [clojure.pprint :refer [pprint]]))

(def crossover-rate 0.7)
(def mutation-rate 0.001)

(def population-size 100)
(def gene-length 300)
(def codon-length 4)

(defn p-check [rate] (< (rand 1) rate))

(defn roulette [gene-pool fitness-fn]
  (let [total-fitness (apply + (map fitness-fn gene-pool))
        slice         (rand total-fitness)]
    (loop [fitness-so-far 0
           gene-pool      (seq gene-pool)]
      (if-not (seq gene-pool)
        ::error
        (let [gene    (first gene-pool)
              fitness (+ fitness-so-far (fitness-fn gene))]
          (if (>= fitness slice)
            gene
            (recur fitness (next gene-pool))))))))

(defn crossover [a b]
  (let [pos (rand (min (count a) (count b)))]
    [(concat (take pos a) (drop pos b))
     (concat (drop pos a) (take pos b))]))

(defn mutate [gene mutation-rate base-mutation-fn]
  (reduce
   (fn [mutated codon]
     (if (p-check mutation-rate)
       (conj mutated (base-mutation-fn codon))
       (conj mutated codon)))
   [] gene))

(defn next-generation [gene-pool fitness-fn crossover-rate mutation-rate base-mutation-fn]
  (loop [this-gen (set gene-pool)
         next-gen #{}]
    (if-not (seq this-gen)
      next-gen
      (let [parent1                 (roulette this-gen fitness-fn)
            parent2                 (roulette (disj this-gen parent1) fitness-fn)
            [offspring1 offspring2]
            (map
             #(mutate % mutation-rate base-mutation-fn)
             (if (p-check crossover-rate)
               (crossover parent1 parent2)
               [parent1 parent2]))]
        (recur (disj this-gen parent1 parent2)
               (conj next-gen offspring1 offspring2))))))

;;------

(def encode
  {0       [0 0 0 0]
   1       [0 0 0 1]
   2       [0 0 1 0]
   3       [0 0 1 1]
   4       [0 1 0 0]
   5       [0 1 0 1]
   6       [0 1 1 0]
   7       [0 1 1 1]
   8       [1 0 0 0]
   9       [1 0 0 1]
   :plus   [1 0 1 0]
   :minus  [1 0 1 1]
   :star   [1 1 0 0]
   :divide [1 1 0 1]})

(def decode (reduce-kv (fn [m k v] (assoc m v k)) {} encode))
(defn parse-gene [gene]
  (reduce
   (fn [result [op num]]
     (cond
       (not (keyword op)) result
       (not (number? num)) result
       (and (= op :divide)
            (zero? num)) (conj result :plus num)
       :else (conj result op num)))
   [] (->> gene
           (partition codon-length)
           (map decode)
           (remove nil?)
           (partition 2))))

(defn parsed-gene->result [parsed]
  (float
   (reduce
    (fn [result [op num]]
      (condp = op
        :plus   (+ result num)
        :minus  (- result num)
        :star   (* result num)
        :divide (/ result num)
        result))
    0 (partition 2 parsed))))

(def fitness-fn
  (memoize
   (fn [gene target]
     (let [target (float target)
           result (-> gene parse-gene parsed-gene->result)]
       (condp = result
         ::invalid 0
         target ::found
         (/ 1 (java.lang.Math/abs (- target result))))))))

(defn random-gene [length]
  (take length (repeatedly #(rand-nth [0 1]))))

(defn random-population [population-size gene-length]
  (take population-size (repeatedly #(random-gene gene-length))))

(defn base-mutation-fn [x] (if (zero? x) 1 0))

(def op-str
  {:plus "+" :minus "-" :star "*" :divide "/"})

(defn gene-str [gene]
  (reduce
   (fn [s [op num]]
     (str "(" s ") " (op-str op) " " num))
   "0" (partition 2 gene)))

(def op-sym
  {:plus '+ :minus '- :star '* :divide '/})

(defn gene-clj [gene]
  (concat
   ['-> 0]
   (map (fn [[op num]]
          (list (op-sym op) num)) (partition 2 gene))))

(defn run [target population-size gene-length fitness-fn crossover-rate mutation-rate base-mutation-fn]
  (loop [generation-count 1
         gene-pool        (random-population population-size gene-length)]
    (println "generation" generation-count
             "- best:"  (apply max (remove #(= % ::found)
                                           (map #(fitness-fn % target) gene-pool))))
    (if (some #(= ::found (fitness-fn % target)) gene-pool)
      (pprint
       (map (comp gene-clj parse-gene) (filter #(= ::found (fitness-fn % target)) gene-pool)))
      (recur
       (inc generation-count)
       (next-generation gene-pool #(fitness-fn % target) crossover-rate mutation-rate base-mutation-fn)))))

(comment
  (run 55 population-size gene-length fitness-fn crossover-rate mutation-rate base-mutation-fn))
