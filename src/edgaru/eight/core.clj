(ns edgaru.eight.core
  (:require [clj-time.core :as tc]
            [clj-time.coerce :as tco]))

;;
(defn random-in-range [lower upper]

  (let [r (+ (rand (- upper lower))
             lower)]

    (if (> r upper)
      upper r)))


(defn stochastic-k [last-price low-price high-price]
  (let [hlrange (- high-price (if (> low-price 0)
                                low-price
                                0))
        hlmidpoint (/ hlrange 2)
        numerator (if (> last-price hlmidpoint)
                    (- last-price hlmidpoint)
                    (- hlmidpoint low-price))]
    (/ numerator hlrange)))


(defn break-local-minima-maxima [k]
  (as-> k k
        (if (<= k 0.05)
          (+ 0.15 k) k)
        (if (>= k 0.2)
          0.2 k)))


(defn generate-prices

  ([low high]
   (generate-prices (random-in-range low high)))

  ([last-price]
   (iterate (fn [{:keys [last]}]

              (let [low (- last 5)
                    high (+ last 5)
                    k (stochastic-k last low high)
                    plus-OR-minus (rand-nth [- +])

                    kPM (if (= plus-OR-minus +)
                          (+ 1 (break-local-minima-maxima k))
                          (- 1 (break-local-minima-maxima k)))

                    newprice (* kPM last)
                    newlow (if (< newprice low) newprice low)
                    newhigh (if (> newprice high) newprice high)]

                ;; (println (str "[" last " | " low " | " high "] <=> k[" k "] / kPM[" kPM "] / newprice[" newprice "]"))
                {:last newprice}))
            {:last last-price})))


(defn generate-timeseries

  ([pricelist]
   (generate-timeseries pricelist (tc/now)))

  ([pricelist datetime]
   (->> (map (fn [x y] [x y])
             (map (fn [x] {:last-trade-time (tco/to-date x)}) (iterate #(tc/plus % (tc/seconds (rand 4))) datetime))
             (map (fn [x] {:last-trade-price x}) pricelist))
        (map (fn [x] (merge (first x) (second x)))))))


(comment


  ;; generate a raw infinite list of floats within a given range
  (def pricelist (generate-prices 5 15))


  ;; generate a timeseries based on thoe numbers
  (def timeseries (take 10 (generate-timeseries pricelist))))
