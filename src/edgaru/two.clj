(ns edgaru.two
  (:require [taoensso.timbre :as timbre]))


`(rand 35)` or `(rand 99)` or `(rand 43.21)`



;;
(repeatedly (fn [] (rand 35)))


;;
`(conj [1 2 3] 4)` and `(conj ‘(1 2 3) 4)`


;;
(map inc (take 100 (repeatedly (fn [] (rand 35)))))


;;
(reduce + (take 100 (repeatedly (fn [] (rand 35)))))


;;
`(>= 4 9)` and `(>= 9 1)`


;;
(filter (fn [x] (>= x 12))
        (repeatedly (fn [] (rand 35))))


;;
(defn generate-prices [lower-bound upper-bound]
    (filter (fn [x] (>= x lower-bound))
              (repeatedly (fn [] (rand upper-bound)))))


;;
(def pricelist (c/generate-prices 12 35))
(map (fn [x] {:price x}) pricelist)


;;
(map (fn [x y]
       [x y])
     (map (fn [x] {:time x}) (iterate inc 0))
     (map (fn [x] {:price x}) pricelist))


;;
(->> (map (fn [x y]
            [x y])
          (map (fn [x] {:time x}) (iterate inc 0))
          (map (fn [x] {:price x}) pricelist))
     (map (fn [x] (merge (first x) (second x)))))


;;
(ns edgar.core)

(defn generate-prices [lower-bound upper-bound]
  (filter (fn [x] (>= x lower-bound))
          (repeatedly (fn [] (rand upper-bound)))))

(defn generate-timeseries [pricelist]
  (->> (map (fn [x y]
              [x y])
            (map (fn [x] {:time x}) (iterate inc 0))
            (map (fn [x] {:price x}) pricelist))
       (map (fn [x] (merge (first x) (second x))))))


;;
(require '[edgar.core :as c])
(def pricelist (c/generate-prices 12 35))
(c/generate-timeseries pricelist)


;;
(defn random-in-range [lower upper]

  (let [r (+ (rand (- upper lower))
             lower)]

    (if (> r upper)
      upper r)))

(defn stochastic-k [last-price low-price high-price]
  (let[hlrange (- high-price low-price)
       hlmidpoint (/ hlrange 2)
       numerator (if (> last-price hlmidpoint)
                   (- last-price hlmidpoint)
                   (- hlmidpoint low-price))]
    (/ numerator hlrange)))

(defn break-local-minima-maxima [k]
  (as-> k k
    (if (<= (int (+ 0.95 k)) 0)
      (+ 0.15 k) k)
    (if (>= k 1)
      (- k 0.15) k)))

(defn generate-prices
  
  ([low high]
   (generate-prices (random-in-range low high) low high))
  
  ([last-price low high]
   (iterate (fn [{:keys [last lows highs]}]
              
              (let [low (-> lows first)
                    high (-> highs reverse first)
                    k (stochastic-k last low high)
                    plus-OR-minus (rand-nth [- +])
                    
                    kPM (if (< k 0.5)
                          (if (= plus-OR-minus +)
                            (+ 1 (break-local-minima-maxima k))
                            (- 1 (break-local-minima-maxima k)))
                          (if (= plus-OR-minus +)
                            (+ 1 (- 1 (break-local-minima-maxima k)))
                            (- 1 (- 1 (break-local-minima-maxima k)))))
                    
                    newprice (* kPM last)
                    newlow (if (< newprice low) newprice low)
                    newhigh (if (> newprice high) newprice high)]
                
                (println (str "[" last " | " low " | " high "] <=> k[" k "] / kPM[" kPM "] / newprice[" newprice "]"))
                {:last newprice
                 :lows (take 5 (conj lows newlow))
                 :highs (take 5 (conj highs newhigh))}))
            
            {:last last-price :lows [low] :highs [high]})))


(defn generate-timeseries
  
  ([pricelist]
   (generate-timeseries pricelist (t/now)))
  
  ([pricelist datetime]
   (->> (map (fn [x y] [x y])
             (map (fn [x] {:time x}) (iterate #(t/plus % (t/seconds (rand 4))) datetime))
             (map (fn [x] {:price x}) pricelist))
        (map (fn [x] (merge (first x) (second x)))))))]"]"]"]")))))))

