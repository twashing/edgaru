(ns edgaru.seven.analytics
  (:require [edgaru.seven.core :as core]))


;; Refactor price-list
(declare timeseries)
(def price-list (core/generate-prices 5 15))
'({:last 10.978625695681702, :lows [5], :highs [15]}
  {:last 15.393542022616002,
   :lows (5 5),
   :highs (15 15.393542022616002)}
  {:last 15.68497326182313,
   :lows (5 5 5),
   :highs (15.68497326182313 15 15.393542022616002)}
  {:last 17.894866781714637,
   :lows (5 5 5 5),
   :highs (17.894866781714637 15.68497326182313 15 15.393542022616002)}
  {:last 19.178454686228328,
   :lows (5 5 5 5 5),
   :highs
   (19.178454686228328
    17.894866781714637
    15.68497326182313
    15
    15.393542022616002)})

(defn extract-price-only [price-list]
  (map :last price-list))

(defn generate-prices-without-population [beginning-low beginning-high]
  (extract-price-only (core/generate-prices beginning-low beginning-high)) )

(def price-only-list (extract-price-only (core/generate-prices 5 15)))
'(10.978625695681702
  15.393542022616002
  15.68497326182313
  17.894866781714637
  19.178454686228328)

(def time-series (core/generate-timeseries price-only-list))

'({:last-trade-price 10.774174002394385,
   :last-trade-time #inst "2015-06-27T17:54:37.583Z"}
  {:last-trade-price 6.221195542189912,
   :last-trade-time #inst "2015-06-27T17:54:40.583Z"}
  {:last-trade-price 6.98092516851132,
   :last-trade-time #inst "2015-06-27T17:54:40.583Z"}
  {:last-trade-price 5.5980561319315,
   :last-trade-time #inst "2015-06-27T17:54:40.583Z"}
  {:last-trade-price 5.263260952271663,
   :last-trade-time #inst "2015-06-27T17:54:42.583Z"})


;; Destructuring
(let [{input-key :input
       output-key :output
       etal-keys :etal
       :or {input-key :last-trade-price
            output-key :last-trade-price-exponential
            etal-keys [:last-trade-price :last-trade-time]}}
      {:input :input-key
       :output :output-key
       :etal [:one :two]}]

  (println input-key)
  (println output-key)
  (println etal-keys))


(let [{input-key :input
       output-key :output
       etal-keys :etal
       :or {input-key :last-trade-price
            output-key :last-trade-price-exponential
            etal-keys [:last-trade-price :last-trade-time]}}
      nil]

  (println input-key)
  (println output-key)
  (println etal-keys))


;; Refactor simple-moving-average
(partition 20 1 (range 40))
'((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
 (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
 (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)
 (3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22)
 (4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23)
 (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24)
 (6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)
 (7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26)
 (8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27)
 (9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28)
 (10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)
 (11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30)
 (12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31)
 (13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32)
 (14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33)
 (15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34)
 (16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35)
 (17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36)
 (18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37)
 (19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38)
 (20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39))


(defn simple-moving-average
  [options tick-window tick-list]

  (let [start-index tick-window

        {input-key :input
         output-key :output
         etal-keys :etal
         :or {input-key :last-trade-price
              output-key :last-trade-price-average
              etal-keys [:last-trade-price :last-trade-time]}} options]

    (reduce (fn [rslt ech]

              (let [tsum (reduce (fn [rr ee]
                                   (let [ltprice (:last-trade-price ee)]
                                     (+ ltprice rr))) 0 ech)

                    taverage (/ tsum (count ech))]

                (lazy-cat rslt
                          [(merge
                             (zipmap etal-keys
                                     (map #(% (last ech)) etal-keys))
                             {output-key taverage
                              :population ech})])))
            '()
            (partition tick-window
                       1
                       (take (* 2 tick-window) tick-list)))))


(defn exponential-moving-average
  "From a tick-list, generates an accompanying exponential moving average list.
     EMA = price(today) * k + EMA(yesterday) * (1 - k)
     k = 2 / N + 1
     N = number of days
   Returns a list, equal in length to the tick-list, but only with slots filled,
   where preceding tick-list allows.
   Options are:
   :input - input key function will look for (defaults to :last-trade-price)
   :output - output key function will emit (defaults to :last-trade-price-exponential)
   :etal - other keys to emit in each result map
   ** This function assumes the latest tick is on the left**"

  ([options tick-window tick-list]
   (exponential-moving-average options tick-window tick-list (simple-moving-average {} tick-window tick-list)))

  ([options tick-window tick-list sma-list]

   ;; 1. calculate 'k'
   ;; k = 2 / N + 1
   ;; N = number of days
   (let [k (/ 2 (+ tick-window 1))

         {input-key :input
          output-key :output
          etal-keys :etal
          :or {input-key :last-trade-price
               output-key :last-trade-price-exponential
               etal-keys [:last-trade-price :last-trade-time]}} options]

     ;; 2. get the simple-moving-average for a given tick - 1
     (reduce (fn [rslt ech]

               ;; 3. calculate the EMA ( for the first tick, EMA(yesterday) = MA(yesterday) )
               (let [;; price(today)
                     ltprice (input-key ech)

                     ;; EMA(yesterday)
                     ema-last (if (output-key (first rslt))
                                (output-key (first rslt))
                                (input-key ech))

                     ;; ** EMA now = price(today) * k + EMA(yesterday) * (1 - k)
                     ema-now (+ (* k ltprice)
                                (* ema-last (- 1 k)))]

                 (lazy-cat rslt
                           [(merge
                             (zipmap etal-keys
                                     (map #(% (last (:population ech))) etal-keys))
                             {output-key ema-now})])))
             '()
             sma-list))))


(defn bollinger-band
  "From a tick-list, generates an accompanying list with upper-band and lower-band
  Upper Band: K times an N-period standard deviation above the moving average (MA + Kσ)
  Lower Band: K times an N-period standard deviation below the moving average (MA − Kσ)
  K: number of standard deviations
  N: period, or tick-window we are looking at
  Returns a list, equal in length to the tick-list, but only with slots filled,
  where preceding tick-list allows.
  ** This function assumes the latest tick is on the left**"

  ([tick-window tick-list]
   (bollinger-band tick-window tick-list (simple-moving-average nil tick-window tick-list)))

  ([tick-window tick-list sma-list]

   ;; At each step, the Standard Deviation will be: the square root of the variance (average of the squared differences from the Mean)
   (reduce (fn [rslt ech]

             (let [;; get the Moving Average
                   ma (:last-trade-price-average ech)

                   ;; work out the mean
                   mean (/ (reduce (fn [rslt ech]
                                     (+ (:last-trade-price ech)
                                        rslt))
                                   0
                                   (:population ech))
                           (count (:population ech)))

                   ;; Then for each number: subtract the mean and square the result (the squared difference)
                   sq-diff-list (map (fn [ech]
                                       (let [diff (- mean (:last-trade-price ech))]
                                         (* diff diff)))
                                     (:population ech))

                   variance (/ (reduce + sq-diff-list) (count (:population ech)))
                   standard-deviation (. Math sqrt variance)]

               (lazy-cat rslt
                         [{:last-trade-price (:last-trade-price ech)
                           :last-trade-time (:last-trade-time ech)
                           :upper-band (+ ma (* 2 standard-deviation))
                           :lower-band (- ma (* 2 standard-deviation))}])))
           '()
           sma-list)))


(comment

  (def price-only-list (extract-price-only (core/generate-prices 5 15)))
  (def time-series (core/generate-timeseries price-only-list))

  (def sma-list (simple-moving-average {} 20 time-series))
  (def ema-list (exponential-moving-average {} 20 time-series sma-list))
  (def bol-band (bollinger-band 20 time-series sma-list))

  )
