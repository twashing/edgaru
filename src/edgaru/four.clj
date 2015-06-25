(ns edgaru.four
  (:require [edgaru.three :as thr]))


(reduce (fn [rslt ech]

                     (let [etal-keys [:last-trade-price :last-trade-time]
                           output-key :output
                           tsum (reduce (fn [rslt inp]

                                          (println (str rslt " <==> " inp))
                                          (let [ltprice (:last (:last-trade-price inp))]
                                            (+ ltprice rslt))) 0 ech)
                           taverage (/ tsum (count ech)) ]

                       (cons (merge

                              (zipmap etal-keys
                                      (map #(% (first ech)) etal-keys))


                              {output-key taverage
                               :population ech})
                             rslt)))

                   (into '() (repeat 20 nil))
                   (reverse (partition 20 1 timeseries)))

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

   (exponential-moving-average options tick-window tick-list (thr/simple-moving-average nil tick-window tick-list)))

  ([options tick-window tick-list sma-list]

   ;; 1. calculate 'k'
   ;; k = 2 / N + 1
   ;; N = number of days
   (let [k (/ 2 (+ tick-window 1))
         ema-list (into '() (repeat tick-window nil))

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
                     ema-now (+ (* k (if (string? ltprice)
                                       (read-string ltprice)
                                       ltprice))
                                (* (if (string? ema-last) (read-string ema-last) ema-last) (- 1 k)))]

                 (cons (merge

                        ;; will produce a map of etal-keys, with associated values in ech
                        (zipmap etal-keys
                                (map #(% ech) etal-keys))

                        ;; and merge the output key to the map
                        {output-key ema-now})

                       ;; and prepend the result to our running list
                       rslt)))
             ema-list
             (->> sma-list (remove nil?) reverse)))))

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
   (bollinger-band tick-window tick-list (thr/simple-moving-average nil tick-window tick-list)))

  ([tick-window tick-list sma-list]

   ;; At each step, the Standard Deviation will be: the square root of the variance (average of the squared differences from the Mean)

   (let [bollinger-list (into '() (repeat tick-window nil))]

     (reduce (fn [rslt ech]

               (let [;; get the Moving Average
                     ma (:last-trade-price-average ech)

                     ;; work out the mean
                     mean (/ (reduce (fn [rslt ech]
                                       (+ (if (string? (:last-trade-price ech))
                                            (read-string (:last-trade-price ech))
                                            (:last-trade-price ech))
                                          rslt))
                                     0
                                     (:population ech))
                             (count (:population ech)))

                     ;; Then for each number: subtract the mean and square the result (the squared difference)
                     sq-diff-list (map (fn [ech]
                                         (let [diff (- mean (if (string? (:last-trade-price ech))
                                                              (read-string (:last-trade-price ech))
                                                              (:last-trade-price ech)))]
                                           (* diff diff)))
                                       (:population ech))

                     variance (/ (reduce + sq-diff-list) (count (:population ech)))
                     standard-deviation (. Math sqrt variance)]
                 (cons {:last-trade-price (:last-trade-price ech)
                        :last-trade-time (:last-trade-time ech)
                        :upper-band (+ ma (* 2 standard-deviation))
                        :lower-band (- ma (* 2 standard-deviation))}
                       rslt)))
             bollinger-list
             (->> sma-list (remove nil?) reverse)))))
