(ns edgaru.core
  (:require [alembic.still]
            [clojure.pprint :as pp]
            [clj-time.core :as t]
            [clj-time.periodic :as p]))


(defn reload-project []
  (alembic.still/load-project))

(defn generate-prices

  ([upper-bound]
   (generate-prices 0 upper-bound))

  ([lower-bound upper-bound]
   (filter #(>= % lower-bound)
           (repeatedly #(rand upper-bound)))))

(defn generate-timeseries

  ([pricelist]
   (generate-timeseries pricelist (t/now)))

  ([pricelist datetime]
   (->> (map (fn [x y] [x y]) (map (fn [x] {:time x}) (iterate #(t/plus % (t/seconds (rand 4))) datetime))
             (map (fn [x] {:price x}) pricelist))
        (map (fn [x] (merge (first x) (second x)))))))

(defn seque-timeseries

  ([timeseries]
   (seque-timeseries timeseries println))

  ([timeseries handlefn]

   (let [start (System/nanoTime)
         sequed-series (seque timeseries)]

     (loop [ech (first sequed-series)
            sseries (rest sequed-series)]

       (let [t1 (:time ech)
             t2 (:time (first sseries))
             tdiff (t/in-millis (t/interval t1 t2))]

         (Thread/sleep tdiff)
         (handlefn ech)
         (if-not (empty? sseries)
           (recur (first sseries)
                  (rest sseries))))))))


(comment

  ;; generate a raw infinite list of floats within a given range
  (def pricelist (generate-prices 5 15))

  ;; => (14.425231029100223 12.561596873601552 9.015979284812179 7.20001565607321 5.001856661709986 11.495735016910736 8.726872997660886 14.28593925643085 6.002666920761813 10.198040792662994 ...)


  ;; generate a timeseries based on thoe numbers
  (def timeseries (take 10 (generate-timeseries pricelist)))

  ;; => ({:price 5.852272525350649, :time #<DateTime 2015-03-29T21:30:51.597Z>} {:price 8.208107304603185, :time #<DateTime 2015-03-29T21:30:54.597Z>} {:price 6.699840469547221, :time #<DateTime 2015-03-29T21:30:57.597Z>} {:price 14.948099116071878, :time #<DateTime 2015-03-29T21:31:00.597Z>} {:price 6.464008119974569, :time #<DateTime 2015-03-29T21:31:03.597Z>} {:price 10.730217701375798, :time #<DateTime 2015-03-29T21:31:03.597Z>} {:price 5.79199822748032, :time #<DateTime 2015-03-29T21:31:04.597Z>} {:price 6.868313897063726, :time #<DateTime 2015-03-29T21:31:04.597Z>} {:price 10.747576562437228, :time #<DateTime 2015-03-29T21:31:07.597Z>} {:price 8.084101904890087, :time #<DateTime 2015-03-29T21:31:10.597Z>})


  ;; execute a side-effecting function over the time series
  (seque-timeseries timeseries)

  )
