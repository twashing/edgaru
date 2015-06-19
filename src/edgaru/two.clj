(ns edgaru.two
  (:require [taoensso.timbre :as timbre]
            [clj-time.core :as t]
            [clj-time.periodic :as p]
            [edgaru.one :as one))


;;
(rand 35)
(rand 99)
(rand 43.21)


;;
(repeatedly (fn [] (rand 35)))


;;
(conj [1 2 3] 4)
(conj '(1 2 3) 4)


;;
(map inc (take 100 (repeatedly (fn [] (rand 35)))))


;;
(reduce + (take 100 (repeatedly (fn [] (rand 35)))))


;;
(>= 4 9)
(>= 9 1)


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
        (map (fn [x] (merge (first x) (second x)))


;;
(defn random-in-range [lower upper]

  (let [r (+ (rand (- upper lower))
             lower)]

    (if (> r upper)
      upper r)))

(defn stochastic-k [last-price low-price high-price]
  (/ (- last-price low-price)
     (- high-price low-price)))

(defn k-plusORMinus [k plus-OR-minus]

  (as-> (if (< k 0.5) (plus-OR-minus 1 k) (Math/abs (plus-OR-minus k 1))) rS
        (if (= (int rS) 0)
            (+ 0.15 rS)
            rS)
        (if (= (int rS) 1)
          (plus-OR-minus rS 0.15) rS)))


(defn generate-newlow [newprice low high]
  (if (< newprice low)
    newprice
    low))

(defn generate-newhigh [newprice low high]
  (if (> newprice high)
    newprice
    high))

(defn generate-prices

  ([low high]
   (generate-prices (random-in-range low high) low high))

  ([last-price low high]

   (iterate (fn [{:keys [last lows highs]}]

              (let [low (-> lows first)
                    high (-> highs reverse first)
                    k (stochastic-k last low high)
                    plus-OR-minus (rand-nth [- +])
                    kPM (k-plusORMinus k plus-OR-minus)

                    newprice (* kPM last)
                    newlow (generate-newlow newprice low high)
                    newhigh (generate-newhigh newprice low high)]

                (timbre/debug (str "k[" k "] / kPM[" kPM "] / newprice[" newprice "] <=> [" last " | " low " | " high "]"))
                {:last newprice
                 :lows (into [] (take 5 (conj lows newlow)))
                 :highs (into [] (take 5 (conj highs newhigh)))}))

            {:last last-price :lows [low] :highs [high]})))

(defn generate-timeseries

  ([pricelist]
   (generate-timeseries pricelist (t/now)))

  ([pricelist datetime]
   (->> (map (fn [x y] [x y])
             (map (fn [x] {:time x}) (iterate #(t/plus % (t/seconds (rand 4))) datetime))
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


  (timbre/set-level! :debug)


  ;; generate a raw infinite list of floats within a given range
  (def pricelist (generate-prices 5 15))


  ;; generate a timeseries based on thoe numbers
  (def timeseries (take 10 (generate-timeseries pricelist)))


  ;; execute a side-effecting function over the time series
  (seque-timeseries timeseries)

  (filter (fn [x] (>= x 12))
          (repeatedly (fn [] (rand 35)))))
