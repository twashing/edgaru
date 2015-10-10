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


(defn generate-timeseries [pricelist]
  (map (fn [x y]
         {:time x :price y})
       (iterate inc 0)
       pricelist))


;;
(require '[edgaru.two.core :as t])
(def pricelist (t/generate-prices 12 35))  ;; pricelist looks something like ((15.400851964198912 23.20772287178392 ...)
(t/generate-timeseries pricelist)  ;; yields (({:price 15.400851964198912, :time 0} {:price 23.20772287178392, :time 1} ...)


;;
(defn random-in-range [lower upper]

  (let [r (rand upper)]
    (if (>= r lower)
      r
      (+ (rand (- upper lower))
         lower))))


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

   (map (fn [x y]
           {:last-trade-time (tco/to-date x)
            :last-trade-price y})
        (iterate #(tc/plus % (tc/seconds (rand 4))) datetime)
        pricelist)))


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


  (set! *print-length* nil)

  ;; execute a side-effecting function over the time series
  (seque-timeseries timeseries)

  (filter (fn [x] (>= x 12))
          (repeatedly (fn [] (rand 35))))


  ;; Chapter 2
  (def pricelist '({:last 9.630404780500577} {:last 7.704323824400461} {:last 6.1634590595203695} {:last 6.880552287598556} {:last 8.174476122037051} {:last 9.809371346444461} {:last 7.84749707715557} {:last 9.416996492586684} {:last 11.30039579110402} {:last 13.560474949324822} {:last 10.848379959459859} {:last 13.01805595135183} {:last 15.621667141622195} {:last 12.497333713297756} {:last 9.997866970638206} {:last 11.997440364765847} {:last 14.396928437719016} {:last 17.276314125262818} {:last 20.731576950315382} {:last 16.585261560252306} {:last 13.268209248201845} {:last 15.921851097842215} {:last 19.106221317410657} {:last 22.92746558089279} {:last 27.512958697071348} {:last 22.01036695765708} {:last 17.608293566125663} {:last 21.129952279350796} {:last 16.90396182348064} {:last 20.284754188176766} {:last 24.341705025812118} {:last 29.21004603097454} {:last 23.368036824779633} {:last 18.694429459823706} {:last 22.433315351788448} {:last 26.919978422146137} {:last 32.30397410657536} {:last 25.84317928526029} {:last 20.674543428208235} {:last 24.809452113849883}))

  (let [prices (map :last pricelist)
        cnt (range (count pricelist))]
    (save (line-chart cnt prices
                      :y-label "Prices"
                      :x-label "Time Interval")
          "Chapter-2-pricelist.png"))


  (def timeseries '({:last-trade-time #inst "2015-10-10T16:27:09.499-00:00", :last-trade-price {:last 9.630404780500577}} {:last-trade-time #inst "2015-10-10T16:27:10.499-00:00", :last-trade-price {:last 7.704323824400461}} {:last-trade-time #inst "2015-10-10T16:27:10.499-00:00", :last-trade-price {:last 6.1634590595203695}} {:last-trade-time #inst "2015-10-10T16:27:12.499-00:00", :last-trade-price {:last 6.880552287598556}} {:last-trade-time #inst "2015-10-10T16:27:15.499-00:00", :last-trade-price {:last 8.174476122037051}} {:last-trade-time #inst "2015-10-10T16:27:15.499-00:00", :last-trade-price {:last 9.809371346444461}} {:last-trade-time #inst "2015-10-10T16:27:17.499-00:00", :last-trade-price {:last 7.84749707715557}} {:last-trade-time #inst "2015-10-10T16:27:17.499-00:00", :last-trade-price {:last 9.416996492586684}} {:last-trade-time #inst "2015-10-10T16:27:17.499-00:00", :last-trade-price {:last 11.30039579110402}} {:last-trade-time #inst "2015-10-10T16:27:19.499-00:00", :last-trade-price {:last 13.560474949324822}} {:last-trade-time #inst "2015-10-10T16:27:20.499-00:00", :last-trade-price {:last 10.848379959459859}} {:last-trade-time #inst "2015-10-10T16:27:22.499-00:00", :last-trade-price {:last 13.01805595135183}} {:last-trade-time #inst "2015-10-10T16:27:24.499-00:00", :last-trade-price {:last 15.621667141622195}} {:last-trade-time #inst "2015-10-10T16:27:26.499-00:00", :last-trade-price {:last 12.497333713297756}} {:last-trade-time #inst "2015-10-10T16:27:28.499-00:00", :last-trade-price {:last 9.997866970638206}} {:last-trade-time #inst "2015-10-10T16:27:30.499-00:00", :last-trade-price {:last 11.997440364765847}} {:last-trade-time #inst "2015-10-10T16:27:31.499-00:00", :last-trade-price {:last 14.396928437719016}} {:last-trade-time #inst "2015-10-10T16:27:33.499-00:00", :last-trade-price {:last 17.276314125262818}} {:last-trade-time #inst "2015-10-10T16:27:36.499-00:00", :last-trade-price {:last 20.731576950315382}} {:last-trade-time #inst "2015-10-10T16:27:37.499-00:00", :last-trade-price {:last 16.585261560252306}} {:last-trade-time #inst "2015-10-10T16:27:37.499-00:00", :last-trade-price {:last 13.268209248201845}} {:last-trade-time #inst "2015-10-10T16:27:37.499-00:00", :last-trade-price {:last 15.921851097842215}} {:last-trade-time #inst "2015-10-10T16:27:39.499-00:00", :last-trade-price {:last 19.106221317410657}} {:last-trade-time #inst "2015-10-10T16:27:41.499-00:00", :last-trade-price {:last 22.92746558089279}} {:last-trade-time #inst "2015-10-10T16:27:44.499-00:00", :last-trade-price {:last 27.512958697071348}} {:last-trade-time #inst "2015-10-10T16:27:44.499-00:00", :last-trade-price {:last 22.01036695765708}} {:last-trade-time #inst "2015-10-10T16:27:47.499-00:00", :last-trade-price {:last 17.608293566125663}} {:last-trade-time #inst "2015-10-10T16:27:50.499-00:00", :last-trade-price {:last 21.129952279350796}} {:last-trade-time #inst "2015-10-10T16:27:53.499-00:00", :last-trade-price {:last 16.90396182348064}} {:last-trade-time #inst "2015-10-10T16:27:56.499-00:00", :last-trade-price {:last 20.284754188176766}} {:last-trade-time #inst "2015-10-10T16:27:56.499-00:00", :last-trade-price {:last 24.341705025812118}} {:last-trade-time #inst "2015-10-10T16:27:57.499-00:00", :last-trade-price {:last 29.21004603097454}} {:last-trade-time #inst "2015-10-10T16:27:58.499-00:00", :last-trade-price {:last 23.368036824779633}} {:last-trade-time #inst "2015-10-10T16:28:01.499-00:00", :last-trade-price {:last 18.694429459823706}} {:last-trade-time #inst "2015-10-10T16:28:04.499-00:00", :last-trade-price {:last 22.433315351788448}} {:last-trade-time #inst "2015-10-10T16:28:05.499-00:00", :last-trade-price {:last 26.919978422146137}} {:last-trade-time #inst "2015-10-10T16:28:06.499-00:00", :last-trade-price {:last 32.30397410657536}} {:last-trade-time #inst "2015-10-10T16:28:09.499-00:00", :last-trade-price {:last 25.84317928526029}} {:last-trade-time #inst "2015-10-10T16:28:11.499-00:00", :last-trade-price {:last 20.674543428208235}} {:last-trade-time #inst "2015-10-10T16:28:11.499-00:00", :last-trade-price {:last 24.809452113849883}}))

  (defn dates->secs [dates]
    (let [[start-time & _ :as long-times] (map tco/to-long dates)]
      (map #(/ (- % start-time) 1000) long-times))) ; time in seconds

  (defn save-timeseries [timeseries filename]
    (let [times (dates->secs (map :last-trade-time timeseries))
          prices (map (comp :last :last-trade-price) timeseries)]
      (save (line-chart times prices
                        :y-label "Prices"
                        :x-label "Time Interval")
            filename)))

  (save-timeseries timeseries "Chapter-2-timeseries.png")


  ;; Chapter 3
  (def pricelist '({:last 9.669598757346312} {:last 11.603518508815574} {:last 13.924222210578689} {:last 11.139377768462952} {:last 8.911502214770362} {:last 7.12920177181629} {:last 8.555042126179547} {:last 6.844033700943638} {:last 5.58197082145023} {:last 5.25711640702317} {:last 6.18085295626428} {:last 6.910720804828229} {:last 5.590275003013771} {:last 5.260295043588595} {:last 6.186262172892861} {:last 6.92011505362294} {:last 5.591373344896528} {:last 5.260714429142853} {:last 4.33445284884367} {:last 5.201343418612404} {:last 6.08627055793232} {:last 6.747404209401539} {:last 5.568359957597347} {:last 5.884843240536026} {:last 6.4055596168362685} {:last 5.5052200247700425} {:last 5.783354764497928} {:last 6.236396615453052} {:last 5.46533064865615} {:last 6.539448831540523} {:last 7.546163517803932} {:last 6.036930814243146} {:last 5.4109428557688775} {:last 6.444943115089396} {:last 7.3762007332185195} {:last 5.900960586574816} {:last 5.369307295431284} {:last 6.372995825307496} {:last 5.497986159022567} {:last 4.399496134200094}))


  (def timeseries '({:last-trade-time #inst "2015-10-10T17:30:29.687-00:00", :last-trade-price {:last 9.669598757346312}} {:last-trade-time #inst "2015-10-10T17:30:32.687-00:00", :last-trade-price {:last 11.603518508815574}} {:last-trade-time #inst "2015-10-10T17:30:32.687-00:00", :last-trade-price {:last 13.924222210578689}} {:last-trade-time #inst "2015-10-10T17:30:32.687-00:00", :last-trade-price {:last 11.139377768462952}} {:last-trade-time #inst "2015-10-10T17:30:32.687-00:00", :last-trade-price {:last 8.911502214770362}} {:last-trade-time #inst "2015-10-10T17:30:33.687-00:00", :last-trade-price {:last 7.12920177181629}} {:last-trade-time #inst "2015-10-10T17:30:35.687-00:00", :last-trade-price {:last 8.555042126179547}} {:last-trade-time #inst "2015-10-10T17:30:37.687-00:00", :last-trade-price {:last 6.844033700943638}} {:last-trade-time #inst "2015-10-10T17:30:40.687-00:00", :last-trade-price {:last 5.58197082145023}} {:last-trade-time #inst "2015-10-10T17:30:41.687-00:00", :last-trade-price {:last 5.25711640702317}} {:last-trade-time #inst "2015-10-10T17:30:44.687-00:00", :last-trade-price {:last 6.18085295626428}} {:last-trade-time #inst "2015-10-10T17:30:44.687-00:00", :last-trade-price {:last 6.910720804828229}} {:last-trade-time #inst "2015-10-10T17:30:46.687-00:00", :last-trade-price {:last 5.590275003013771}} {:last-trade-time #inst "2015-10-10T17:30:46.687-00:00", :last-trade-price {:last 5.260295043588595}} {:last-trade-time #inst "2015-10-10T17:30:48.687-00:00", :last-trade-price {:last 6.186262172892861}} {:last-trade-time #inst "2015-10-10T17:30:51.687-00:00", :last-trade-price {:last 6.92011505362294}} {:last-trade-time #inst "2015-10-10T17:30:53.687-00:00", :last-trade-price {:last 5.591373344896528}} {:last-trade-time #inst "2015-10-10T17:30:55.687-00:00", :last-trade-price {:last 5.260714429142853}} {:last-trade-time #inst "2015-10-10T17:30:57.687-00:00", :last-trade-price {:last 4.33445284884367}} {:last-trade-time #inst "2015-10-10T17:30:58.687-00:00", :last-trade-price {:last 5.201343418612404}} {:last-trade-time #inst "2015-10-10T17:30:59.687-00:00", :last-trade-price {:last 6.08627055793232}} {:last-trade-time #inst "2015-10-10T17:31:00.687-00:00", :last-trade-price {:last 6.747404209401539}} {:last-trade-time #inst "2015-10-10T17:31:02.687-00:00", :last-trade-price {:last 5.568359957597347}} {:last-trade-time #inst "2015-10-10T17:31:02.687-00:00", :last-trade-price {:last 5.884843240536026}} {:last-trade-time #inst "2015-10-10T17:31:02.687-00:00", :last-trade-price {:last 6.4055596168362685}} {:last-trade-time #inst "2015-10-10T17:31:03.687-00:00", :last-trade-price {:last 5.5052200247700425}} {:last-trade-time #inst "2015-10-10T17:31:04.687-00:00", :last-trade-price {:last 5.783354764497928}} {:last-trade-time #inst "2015-10-10T17:31:04.687-00:00", :last-trade-price {:last 6.236396615453052}} {:last-trade-time #inst "2015-10-10T17:31:04.687-00:00", :last-trade-price {:last 5.46533064865615}} {:last-trade-time #inst "2015-10-10T17:31:06.687-00:00", :last-trade-price {:last 6.539448831540523}} {:last-trade-time #inst "2015-10-10T17:31:09.687-00:00", :last-trade-price {:last 7.546163517803932}} {:last-trade-time #inst "2015-10-10T17:31:11.687-00:00", :last-trade-price {:last 6.036930814243146}} {:last-trade-time #inst "2015-10-10T17:31:14.687-00:00", :last-trade-price {:last 5.4109428557688775}} {:last-trade-time #inst "2015-10-10T17:31:17.687-00:00", :last-trade-price {:last 6.444943115089396}} {:last-trade-time #inst "2015-10-10T17:31:17.687-00:00", :last-trade-price {:last 7.3762007332185195}} {:last-trade-time #inst "2015-10-10T17:31:18.687-00:00", :last-trade-price {:last 5.900960586574816}} {:last-trade-time #inst "2015-10-10T17:31:21.687-00:00", :last-trade-price {:last 5.369307295431284}} {:last-trade-time #inst "2015-10-10T17:31:24.687-00:00", :last-trade-price {:last 6.372995825307496}} {:last-trade-time #inst "2015-10-10T17:31:24.687-00:00", :last-trade-price {:last 5.497986159022567}} {:last-trade-time #inst "2015-10-10T17:31:25.687-00:00", :last-trade-price {:last 4.399496134200094}}))

  (save-timeseries timeseries "Chapter-3-timeseries.png")


  (require '[edgaru.three.analytics :as analytics])

  (def sma-list (analytics/simple-moving-average {} 20 (take 40 timeseries)))
  (def smalist '({:last-trade-price {:last 9.669598757346312}, :last-trade-time #inst "2015-10-10T17:30:29.687-00:00", :last-trade-price-average 7.3025994681546464} {:last-trade-price {:last 11.603518508815574}, :last-trade-time #inst "2015-10-10T17:30:32.687-00:00", :last-trade-price-average 7.123433058183946} {:last-trade-price {:last 13.924222210578689}, :last-trade-time #inst "2015-10-10T17:30:32.687-00:00", :last-trade-price-average 6.880627343213244} {:last-trade-price {:last 11.139377768462952}, :last-trade-time #inst "2015-10-10T17:30:32.687-00:00", :last-trade-price-average 6.462834230564177} {:last-trade-price {:last 8.911502214770362}, :last-trade-time #inst "2015-10-10T17:30:32.687-00:00", :last-trade-price-average 6.20010750416783} {:last-trade-price {:last 7.12920177181629}, :last-trade-time #inst "2015-10-10T17:30:33.687-00:00", :last-trade-price-average 6.074810374271126} {:last-trade-price {:last 8.555042126179547}, :last-trade-time #inst "2015-10-10T17:30:35.687-00:00", :last-trade-price-average 5.993611286918814} {:last-trade-price {:last 6.844033700943638}, :last-trade-time #inst "2015-10-10T17:30:37.687-00:00", :last-trade-price-average 5.855026918834733} {:last-trade-price {:last 5.58197082145023}, :last-trade-time #inst "2015-10-10T17:30:40.687-00:00", :last-trade-price-average 5.824645064560203} {:last-trade-price {:last 5.25711640702317}, :last-trade-time #inst "2015-10-10T17:30:41.687-00:00", :last-trade-price-average 5.818813055920499} {:last-trade-price {:last 6.18085295626428}, :last-trade-time #inst "2015-10-10T17:30:44.687-00:00", :last-trade-price-average 5.882929677146366} {:last-trade-price {:last 6.910720804828229}, :last-trade-time #inst "2015-10-10T17:30:44.687-00:00", :last-trade-price-average 5.951195205223349} {:last-trade-price {:last 5.590275003013771}, :last-trade-time #inst "2015-10-10T17:30:46.687-00:00", :last-trade-price-average 5.907505705694094} {:last-trade-price {:last 5.260295043588595}, :last-trade-time #inst "2015-10-10T17:30:46.687-00:00", :last-trade-price-average 5.89853909833185} {:last-trade-price {:last 6.186262172892861}, :last-trade-time #inst "2015-10-10T17:30:48.687-00:00", :last-trade-price-average 5.9577715019068895} {:last-trade-price {:last 6.92011505362294}, :last-trade-time #inst "2015-10-10T17:30:51.687-00:00", :last-trade-price-average 6.017268429923173} {:last-trade-price {:last 5.591373344896528}, :last-trade-time #inst "2015-10-10T17:30:53.687-00:00", :last-trade-price-average 5.966310706570768} {:last-trade-price {:last 5.260714429142853}, :last-trade-time #inst "2015-10-10T17:30:55.687-00:00", :last-trade-price-average 5.955207404097505} {:last-trade-price {:last 4.33445284884367}, :last-trade-time #inst "2015-10-10T17:30:57.687-00:00", :last-trade-price-average 6.010821473905738} {:last-trade-price {:last 5.201343418612404}, :last-trade-time #inst "2015-10-10T17:30:58.687-00:00", :last-trade-price-average 6.068998139414683} {:last-trade-price {:last 6.08627055793232}, :last-trade-time #inst "2015-10-10T17:30:59.687-00:00", :last-trade-price-average 6.028905775194066}))

  (defn timeseries->table [timeseries group]
    (let [long-times (dates->secs (map :last-trade-time timeseries))
          prices (map (comp :last :last-trade-price) timeseries)]
      (map #(assoc {} :time % :price %2 :group %3)
           long-times
           prices
           (repeat group))))

  (defn dates->secs [dates]
    (let [[start-time & _ :as long-times] (map tco/to-long dates)]
      (map #(/ (- % start-time) 1000)
           long-times)))

  (defn save-moving-average [smalist filename]
    (let [data (dataset smalist)]

      (save (line-chart (sel data :cols 1)
                        (sel data :cols 0)
                        ;;:group-by (sel data :cols 1)
                        ;;:y-label "Price"
                        ;;:x-label "Time"
                        ;;:legend true
                        )
            filename)))

  (defn fubar [smalist]

    (let [#_one #_(map (fn [x]
                     (update-in x [:last-trade-time] tco/to-long))
                       smalist)

          #_two #_(map (fn [x]
                     (update-in x [:last-trade-price] :last))
                       one)

          times-one (map :last-trade-time smalist)
          times-two (map dates->secs times-one)

          prices (map (comp :last :last-trade-price) smalist)
          averages (map :last-trade-price-average smalist)]

      (map (fn [x y z] {:last-trade-time x :last-trade-price y :last-trade-price-average z})
           times-two
           prices
           averages)))

  (save-moving-average (fubar smalist) "Chapter-3-moving-average.png")


  ;; Chapter 4
  (def sma-coll
    [{:last-trade-price 14.00761245880067,
      :last-trade-time #inst "2015-09-07T00:11:52.679-00:00",
      :last-trade-price-average 6.917349887328998,
      :population
      [{:last-trade-time #inst "2015-09-07T00:11:52.679-00:00",
        :last-trade-price 14.00761245880067}
       {:last-trade-time #inst "2015-09-07T00:11:53.679-00:00",
        :last-trade-price 11.206089967040537}
       {:last-trade-time #inst "2015-09-07T00:11:54.679-00:00",
        :last-trade-price 8.96487197363243}
       {:last-trade-time #inst "2015-09-07T00:11:57.679-00:00",
        :last-trade-price 7.171897578905945}
       {:last-trade-time #inst "2015-09-07T00:11:58.679-00:00",
        :last-trade-price 5.737518063124757}
       {:last-trade-time #inst "2015-09-07T00:11:59.679-00:00",
        :last-trade-price 5.314365742218849}
       {:last-trade-time #inst "2015-09-07T00:12:02.679-00:00",
        :last-trade-price 6.278586056649181}
       {:last-trade-time #inst "2015-09-07T00:12:02.679-00:00",
        :last-trade-price 5.4758147978988205}
       {:last-trade-time #inst "2015-09-07T00:12:03.679-00:00",
        :last-trade-price 6.557734388723003}
       {:last-trade-time #inst "2015-09-07T00:12:06.679-00:00",
        :last-trade-price 5.536213551780478}
       {:last-trade-time #inst "2015-09-07T00:12:09.679-00:00",
        :last-trade-price 5.2393542785789355}
       {:last-trade-time #inst "2015-09-07T00:12:11.679-00:00",
        :last-trade-price 6.150663606722648}
       {:last-trade-time #inst "2015-09-07T00:12:13.679-00:00",
        :last-trade-price 5.442929129777727}
       {:last-trade-time #inst "2015-09-07T00:12:15.679-00:00",
        :last-trade-price 6.500451685533815}
       {:last-trade-time #inst "2015-09-07T00:12:17.679-00:00",
        :last-trade-price 5.525090316704781}
       {:last-trade-time #inst "2015-09-07T00:12:17.679-00:00",
        :last-trade-price 5.815207459126885}
       {:last-trade-time #inst "2015-09-07T00:12:18.679-00:00",
        :last-trade-price 5.341147409421831}
       {:last-trade-time #inst "2015-09-07T00:12:20.679-00:00",
        :last-trade-price 6.3245313810415436}
       {:last-trade-time #inst "2015-09-07T00:12:21.679-00:00",
        :last-trade-price 7.162235409498696}
       {:last-trade-time #inst "2015-09-07T00:12:24.679-00:00",
        :last-trade-price 8.594682491398435}]}
     {:last-trade-price 11.206089967040537,
      :last-trade-time #inst "2015-09-07T00:11:53.679-00:00",
      :last-trade-price-average 6.560756564044901,
      :population
      [{:last-trade-time #inst "2015-09-07T00:11:53.679-00:00",
        :last-trade-price 11.206089967040537}
       {:last-trade-time #inst "2015-09-07T00:11:54.679-00:00",
        :last-trade-price 8.96487197363243}
       {:last-trade-time #inst "2015-09-07T00:11:57.679-00:00",
        :last-trade-price 7.171897578905945}
       {:last-trade-time #inst "2015-09-07T00:11:58.679-00:00",
        :last-trade-price 5.737518063124757}
       {:last-trade-time #inst "2015-09-07T00:11:59.679-00:00",
        :last-trade-price 5.314365742218849}
       {:last-trade-time #inst "2015-09-07T00:12:02.679-00:00",
        :last-trade-price 6.278586056649181}
       {:last-trade-time #inst "2015-09-07T00:12:02.679-00:00",
        :last-trade-price 5.4758147978988205}
       {:last-trade-time #inst "2015-09-07T00:12:03.679-00:00",
        :last-trade-price 6.557734388723003}
       {:last-trade-time #inst "2015-09-07T00:12:06.679-00:00",
        :last-trade-price 5.536213551780478}
       {:last-trade-time #inst "2015-09-07T00:12:09.679-00:00",
        :last-trade-price 5.2393542785789355}
       {:last-trade-time #inst "2015-09-07T00:12:11.679-00:00",
        :last-trade-price 6.150663606722648}
       {:last-trade-time #inst "2015-09-07T00:12:13.679-00:00",
        :last-trade-price 5.442929129777727}
       {:last-trade-time #inst "2015-09-07T00:12:15.679-00:00",
        :last-trade-price 6.500451685533815}
       {:last-trade-time #inst "2015-09-07T00:12:17.679-00:00",
        :last-trade-price 5.525090316704781}
       {:last-trade-time #inst "2015-09-07T00:12:17.679-00:00",
        :last-trade-price 5.815207459126885}
       {:last-trade-time #inst "2015-09-07T00:12:18.679-00:00",
        :last-trade-price 5.341147409421831}
       {:last-trade-time #inst "2015-09-07T00:12:20.679-00:00",
        :last-trade-price 6.3245313810415436}
       {:last-trade-time #inst "2015-09-07T00:12:21.679-00:00",
        :last-trade-price 7.162235409498696}
       {:last-trade-time #inst "2015-09-07T00:12:24.679-00:00",
        :last-trade-price 8.594682491398435}
       {:last-trade-time #inst "2015-09-07T00:12:27.679-00:00",
        :last-trade-price 6.875745993118748}]}])

  (defn timeseries->table-2 [timeseries group]
    (let [long-times (dates->secs (map :last-trade-time timeseries))
          prices (map :last-trade-price timeseries)]
      (map #(assoc {} :time % :price %2 :group %3)
           long-times prices (repeat group))))

  (defn save-sma-coll [coll filename]
    (let [data #spy/d (-> (map (fn [{:keys [population]} grp]
                          (timeseries->table-2 population grp))
                        coll [1 2]) flatten dataset)]
      (save (line-chart (sel data :cols 0)
                        (sel data :cols 1)
                        :group-by (sel data :cols 2)
                        :y-label "Price"
                        :x-label "Time Interval")
            filename)))

  (save-sma-coll sma-coll "Chapter-4-sma-coll.png")

  )
