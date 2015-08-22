(ns edgaru.nine
  (:require [edgaru.two :as two]
            [edgaru.four :as four]
            [edgaru.five :as five]))

(defn join-averages
  "Create a list where i) tick-list ii) sma-list and iii) ema-list are overlaid."

  ([tick-window tick-list]

   (let [sma-list (four/simple-moving-average nil tick-window tick-list)
         ema-list (four/exponential-moving-average nil tick-window tick-list sma-list)]
     (join-averages tick-list sma-list ema-list)))

  ([tick-list sma-list ema-list]

   (let [trimmed-ticks (drop-while #(not (= (:last-trade-time %)
                                            (:last-trade-time (first sma-list))))
                                   tick-list)]

     (map (fn [titem sitem eitem]

            ;; 1. ensure that we have the :last-trade-time for simple and exponential items
            ;; 2. ensure that all 3 time items line up
            (if (and (and (not (nil? (:last-trade-time sitem)))
                          (not (nil? (:last-trade-time eitem))))
                     (= (:last-trade-time titem) (:last-trade-time sitem) (:last-trade-time eitem)))

              {:last-trade-time (:last-trade-time titem)
               :last-trade-price (if (string? (:last-trade-price titem))
                                   (read-string (:last-trade-price titem))
                                   (:last-trade-price titem))
               :last-trade-price-average (:last-trade-price-average sitem)
               :last-trade-price-exponential (:last-trade-price-exponential eitem)}

              nil))

          trimmed-ticks
          sma-list
          ema-list))))


(comment

  ;; 1.
  (def price-list (five/generate-prices))
  (def time-series (two/generate-timeseries price-list))

  (def prices (take 320 time-series))
  (def remaining (drop 320 time-series))

  ;; 2.
  (def sma (four/simple-moving-average {} 20 prices))
  (def ema (four/exponential-moving-average {} 20 prices sma))
  (def bol (four/bollinger-band 20 prices sma))

  (join-averages 20 (take 100 time-series))

  )
