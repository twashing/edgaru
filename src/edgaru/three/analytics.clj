(ns edgaru.three.analytics
  (:require [edgaru.three.core :as core]))


;;
(partition 4 (range 20))


;;
(partition 5 1 (range 10))


;;
(defn moving-average [tick-seq tick-window]
    (partition tick-window 1 tick-seq))


;;
(def pricelist (core/generate-prices 5 15))
(def timeseries (core/generate-timeseries pricelist))
(def movingaverage (moving-average timeseries 20))
(take 2 movingaverage)


;;
(def ^:dynamic x 1)
(def ^:dynamic y 1)

(+ x y)

(binding [x 2 y 3]
    (+ x y))

(+ x y)


;;
(defn simple-moving-average
  "Takes the tick-list, and moves back as far as the tick window will take it.
   Returns a list, equal in length to the tick-list, but only with slots filled,
   where preceding tick-list allows.
   Options are:
   :input - input key function will look for (defaults to :last-trade-price)
   :output - output key function will emit (defaults to :last-trade-price-average)
   :etal - other keys to emit in each result map"
  [options tick-window tick-list]

  (let [;; calculate how far back the window can start
        start-index tick-window

        {input-key :input
         output-key :output
         etal-keys :etal
         :or {input-key :last-trade-price
              output-key :last-trade-price-average
              etal-keys [:last-trade-price :last-trade-time]}} options]


    ;; calculate Simple Moving Average for each slot there's a window
    (reduce (fn [rslt ech]

              (let [tsum (reduce (fn [rslt inp]
                                   (let [ltprice (:last (input-key inp))]

                                     ;; sum it up
                                     (+ ltprice rslt))) 0 ech)

                    ;; get the average
                    taverage (/ tsum (count ech))]

                (conj rslt
                      (merge

                       ;; will produce a map of etal-keys, with associated values in ech
                       (zipmap etal-keys
                               (map #(% (first ech)) etal-keys))

                       ;; and merge the output key to the map
                       {output-key taverage
                        :population ech}))))

            []
            (partition tick-window 1 tick-list))))

;;
(comment

  (def sma-list (simple-moving-average {} 20 (take 40 timeseries)))

  (def one (reduce (fn [rslt ech]

                     (let [etal-keys [:last-trade-price :last-trade-time]
                           output-key :output
                           tsum (reduce (fn [rslt inp]

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
                   (partition 20 1 timeseries)))

  )
