(ns edgaru.three
  (:require [taoensso.timbre :as timbre]
            [edgaru.two :as t]))


;;
(partition 4 (range 20))


;;
(partition 5 1 (range 10))


;;
(defn moving-average [tick-seq tick-window]
    (partition tick-window 1 tick-seq))


;;
(def pricelist (t/generate-prices 5 15))
(def timeseries (t/generate-timeseries pricelist))
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
   :etal - other keys to emit in each result map
   ** This function assumes the latest tick is on the left**"
  [options tick-window tick-list]

  (let [;; calculate how far back the window can start
        start-index tick-window

        ;; back fill slots with nils, into an accompanying moving-average list
        ma-list (into '() (repeat tick-window nil))

        {input-key :input
         output-key :output
         etal-keys :etal
         :or {input-key :last-trade-price
              output-key :last-trade-price-average
              etal-keys [:last-trade-price :last-trade-time]}} options]


    ;; calculate Simple Moving Average for each slot there's a window
    (reduce (fn [rslt ech]

              (let [tsum (reduce (fn [rslt inp]
                                   (let [ltprice (input-key inp)]
                                     (+ (if (string? ltprice) (read-string ltprice) ltprice) rslt))) 0 ech)   ;; sum it up
                    taverage (/ tsum (count ech))   ;; get the average
                    ]

                (cons (merge

                       ;; will produce a map of etal-keys, with associated values in ech
                       (zipmap etal-keys
                               (map #(% (first ech)) etal-keys))

                       ;; and merge the output key to the map
                       {output-key taverage
                        :population ech})
                      rslt)))

            ma-list  ;; begin with a reversed tick-list, over which we can iterate
            (reverse (partition tick-window 1 tick-list)))))

;;
(simple-moving-average {} pricelist 20)
