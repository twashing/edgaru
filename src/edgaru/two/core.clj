(ns edgaru.two.core
  (:require [clj-time.core :as tc]
            [clj-time.periodic :as tp]
            [clj-time.coerce :as tco]))


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
(require '[edgaru.two.core :as t])
;; (require (quote [edgaru.two.core :as t]))
'(a b c)


;;
;;((t1 29.60706184716407) (t2 12.507593971664075))  ;; will produce an error
;;(t1 29.60706184716407)  ;; will produce an error
'((t1 29.60706184716407) (t2 12.507593971664075))


;;
{0 1}
{:a 1}
:a
:fubar
:thing
;;fubar  ;; will produce an error
(def fubar 1)
(def thing 2)
{:fubar 1}
{0 29.60706184716407}
{:time 0 :price 29.60706184716407}


;;
(def pricelist (t/generate-prices 12 35))
(map (fn [x] {:price x}) pricelist)


;;
(map (fn [x y]
       [x y])
     (map (fn [x] {:time x}) (iterate inc 0))
     (map (fn [x] {:price x}) pricelist))


;;
([58 23 4 638] 2)
(get [58 23 4 638] 2)


;;
[{:time 0} {:price 23.113293577419874}]
{:time 0 :price 23.113293577419874}
(merge {:time 0} {:price 23.113293577419874})
{:price 23.113293577419874, :time 0}


;;
(-> 10 (/ 5))
(/ 10 2)
(->> 10 (/ 5))
(/ 5 10)


;;
(->> (map (fn [x y]
            [x y])
          (map (fn [x] {:time x}) (iterate inc 0))
          (map (fn [x] {:price x}) pricelist))
     (map (fn [x] (merge (first x) (second x)))))


;;
(ns edgaru.two.core)

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
(require '[edgaru.two.core :as t])
(def pricelist (t/generate-prices 12 35))  ;; pricelist looks something like ((15.400851964198912 23.20772287178392 ...)
(t/generate-timeseries pricelist)  ;; yields (({:price 15.400851964198912, :time 0} {:price 23.20772287178392, :time 1} ...)


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


;;
(map :last (take 40 (generate-prices 5 15)))


;;
(def pricelist (generate-prices 5 15))
(take 40 (generate-timeseries pricelist))



(comment


  ;; generate a raw infinite list of floats within a given range
  (def pricelist (generate-prices 5 15))


  ;; generate a timeseries based on thoe numbers
  (def timeseries (take 40 (generate-timeseries pricelist)))


  ;; execute a side-effecting function over the time series
  (seque-timeseries timeseries)

  (filter (fn [x] (>= x 12))
          (repeatedly (fn [] (rand 35)))))
