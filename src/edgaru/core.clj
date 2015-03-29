(ns edgaru.core
  (:require [clojure.pprint :as pp]))

(defn generate-prices

  ([upper-bound]
   (generate-timeseries 0 upper-bound))

  ([lower-bound upper-bound]
   (filter #(>= % lower-bound)
           (repeatedly #(rand upper-bound)))))

(defn generate-timeseries [pricelist]

  (->> (zipmap (map (fn [x] {:time x}) (take (count pricelist) (iterate inc 0)))
               (map (fn [x] {:price x}) pricelist))

       (map (fn [x] (merge (first x) (second x))))

       (sort-by :time)))

(comment

  ;; generate a raw list of floats within a given range
  (def plist (take 10 (generate-prices 5 15)))

  ;; => (14.425231029100223 12.561596873601552 9.015979284812179 7.20001565607321 5.001856661709986 11.495735016910736 8.726872997660886 14.28593925643085 6.002666920761813 10.198040792662994)


  ;; generate a timeseries based on thoe numbers
  (def tseries (generate-timeseries plist))

  ;; =>
  #_({:price 6.498531167254335, :time 0}
     {:price 14.932585251457299, :time 1}
     {:price 7.1285417453458795, :time 2}
     {:price 12.81164293198097, :time 3}
     {:price 7.244480692160645, :time 4}
     {:price 13.19382349693832, :time 5}
     {:price 6.261360701619896, :time 6}
     {:price 10.569452494512952, :time 7}
     {:price 9.851295409730925, :time 8}
     {:price 6.778911488317861, :time 9})

  )
