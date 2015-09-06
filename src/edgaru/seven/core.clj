(ns edgaru.seven.core
  (:require [clj-time.core :as tc]
            [clj-time.coerce :as tco]))


;;
(defn random-in-range [lower upper]

  (let [r (+ (rand (- upper lower))
             lower)]

    (if (> r upper)
      upper r)))

(defn stochastic-k [last-price low-price high-price]
  (let [hlrange (- high-price low-price)
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

                ;;(println (str "[" last " | " low " | " high "] <=> k[" k "] / kPM[" kPM "] / newprice[" newprice "]"))
                {:last newprice
                 :lows (take 5 (conj lows newlow))
                 :highs (take 5 (conj highs newhigh))}))

            {:last last-price :lows [low] :highs [high]})))


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
  (def timeseries (take 10 (generate-timeseries pricelist)))


  ;; execute a side-effecting function over the time series
  (seque-timeseries timeseries)


  (filter (fn [x] (>= x 12))
          (repeatedly (fn [] (rand 35)))))
