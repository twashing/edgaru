(ns edgaru.nine.signals
  (:require [edgaru.nine.core :as core]
            [edgaru.nine.analytics :as analytics]
            [edgaru.nine.datasource :as datasource]))


(defn simple-moving-average
  [options tick-window tick-list]

  (let [start-index tick-window

        {input-key :input
         output-key :output
         etal-keys :etal
         :or {input-key :last-trade-price
              output-key :last-trade-price-average
              etal-keys [:last-trade-price :last-trade-time]}} options]

    (map (fn [ech]

           (let [tsum (reduce (fn [rr ee]
                                (let [ltprice (:last-trade-price ee)]
                                  (+ ltprice rr))) 0 ech)

                 taverage (/ tsum (count ech))]

             (merge
              (zipmap etal-keys
                      (map #(% (last ech)) etal-keys))
              {output-key taverage
               :population ech})))
         (partition tick-window 1 tick-list))))


(defn exponential-moving-average
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
     (last (reductions (fn [rslt ech]

                         ;; 3. calculate the EMA ( for the first tick, EMA(yesterday) = MA(yesterday) )
                         (let [;; price(today)
                               ltprice (input-key ech)

                               ;; EMA(yesterday)
                               ema-last (if (output-key (last rslt))
                                          (output-key (last rslt))
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
                       sma-list)))))


(defn bollinger-band
  ([tick-window tick-list]
   (bollinger-band tick-window tick-list (simple-moving-average nil tick-window tick-list)))

  ([tick-window tick-list sma-list]

   ;; At each step, the Standard Deviation will be: the square root of the variance (average of the squared differences from the Mean)
   (map (fn [ech]

          (let [;; get the Moving Average
                ma (:last-trade-price-average ech)

                ;; work out the mean
                mean (/ (reduce (fn [rlt ech]
                                  (+ (:last-trade-price ech)
                                     rlt))
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

            {:last-trade-price (:last-trade-price ech)
             :last-trade-time (:last-trade-time ech)
             :upper-band (+ ma (* 2 standard-deviation))
             :lower-band (- ma (* 2 standard-deviation))}))
        sma-list)))


(defn join-averages
  "Create a list where i) tick-list ii) sma-list and iii) ema-list are overlaid."

  ([tick-window tick-list]

   (let [sma-list (simple-moving-average nil tick-window tick-list)
         ema-list (exponential-moving-average nil tick-window tick-list sma-list)]
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


(defn moving-averages-signals
  "Takes baseline time series, along with 2 other moving averages.
   Produces a list of signals where the 2nd moving average overlaps (abouve or below) the first.
   By default, this function will produce a Simple Moving Average and an Exponential Moving Average."

  ([tick-window tick-list]

   (let [sma-list (simple-moving-average nil tick-window tick-list)
         ema-list (exponential-moving-average nil tick-window tick-list sma-list)]
     (moving-averages-signals tick-list sma-list ema-list)))

  ([tick-list sma-list ema-list]

   ;; create a list where i) tick-list ii) sma-list and iii) ema-list are overlaid
   (let [joined-list (join-averages tick-list sma-list ema-list)
         partitioned-join (partition 2 1 (remove nil? joined-list))]

     ;; find time points where ema-list (or second list) crosses over the sma-list (or 1st list)
     (map (fn [[fst snd]]
            (let [;;fst (first ech)
                  ;;snd (second ech)

                  ;; in the first element, has the ema crossed abouve the sma from the second element
                  signal-up (and (< (:last-trade-price-exponential snd) (:last-trade-price-average snd))
                                 (> (:last-trade-price-exponential fst) (:last-trade-price-average fst)))

                  ;; in the first element, has the ema crossed below the sma from the second element
                  signal-down (and (> (:last-trade-price-exponential snd) (:last-trade-price-average snd))
                                   (< (:last-trade-price-exponential fst) (:last-trade-price-average fst)))

                  raw-data fst]

              ;; return either i) :up signal, ii) :down signal or iii) nothing, with just the raw data
              (if signal-up
                (assoc raw-data :signals [{:signal :up
                                           :why :moving-average-crossover
                                           :arguments [fst snd]}])
                (if signal-down
                  (assoc raw-data :signals [{:signal :down
                                             :why :moving-average-crossover
                                             :arguments [fst snd]}])
                  raw-data))))
          partitioned-join))))


(defn relative-strength-index
  "The Relative Strength Index (RSI) is a momentum oscillator that measures the speed and change of price movements. It oscillates between zero and 100.
   If no 'tick-window' is given, it defaults to 14"
  [tick-window tick-list]

  (let [twindow (if tick-window tick-window 14)
        window-list (partition twindow 1 tick-list)]

    ;; run over the collection of populations
    (last (reductions (fn [rslt ech]

                        ;; each item will be a population of tick-window (default of 14)
                        (let [pass-one (reduce (fn [rslt ech]

                                                 (let [fst (:last-trade-price (first ech))
                                                       snd (:last-trade-price (second ech))

                                                       up? (< fst snd)
                                                       down? (> fst snd)
                                                       sideways? (and (not up?) (not down?))]

                                                   (if (or up? down?)
                                                     (if up?
                                                       (conj rslt (assoc (first ech) :signal :up))
                                                       (conj rslt (assoc (first ech) :signal :down)))
                                                     (conj rslt (assoc (first ech) :signal :sideways)))))
                                               []
                                               (partition 2 1 (remove nil? ech)))


                              up-list (:up (group-by :signal pass-one))
                              down-list (:down (group-by :signal pass-one))

                              avg-gains (/ (apply +
                                                  (map :last-trade-price up-list))
                                           tick-window)
                              avg-losses (/ (apply +
                                                   (map :last-trade-price down-list))
                                            tick-window)

                              rs (if-not (= 0 avg-losses)
                                   (/ avg-gains avg-losses)
                                   0)
                              rsi (- 100 (/ 100 (+ 1 rs)))]

                          (conj rslt {:last-trade-time (:last-trade-time (first ech))
                                      :last-trade-price (:last-trade-price (first ech))
                                      :rs rs
                                      :rsi rsi})))
                      []
                      window-list))))


(defn find-peaks-valleys [options tick-list]

  (let [{input-key :input
         :or {input-key :last-trade-price}} options]

    (reduce (fn [rslt ech]
              (let [fst (input-key (first ech))
                    snd (input-key (second ech))
                    thd (input-key (nth ech 2))

                    valley? (and (and (-> fst nil? not) (-> snd nil? not) (-> thd nil? not))
                                 (> fst snd)
                                 (< snd thd))
                    peak? (and (and (-> fst nil? not) (-> snd nil? not) (-> thd nil? not))
                               (< fst snd)
                               (> snd thd))]

                (if (or valley? peak?)
                  (if peak?
                    (conj rslt (assoc (second ech) :signal :peak))
                    (conj rslt (assoc (second ech) :signal :valley)))
                  rslt)))
            []
            (partition 3 1 tick-list))))


(defn up-market? [period partitioned-list]
  (every? (fn [inp]
            (< (:last-trade-price (first inp))
               (:last-trade-price (second inp))))
          (take period partitioned-list)))


(defn down-market? [period partitioned-list]
  (every? (fn [inp]
            (> (:last-trade-price (first inp))
               (:last-trade-price (second inp))))
          (take period partitioned-list)))


(defn sort-bollinger-band [bband]
  (let [diffs (map (fn [inp] (assoc inp :difference (- (:upper-band inp) (:lower-band inp))))
                   (remove nil? bband))]
    (sort-by :difference diffs)))


(defn calculate-strategy-a [rslt ech-list most-narrow upM? peaks valleys]

  (let [latest-diff (- (:upper-band (first ech-list)) (:lower-band (first ech-list)))
        less-than-any-narrow? (some (fn [inp] (< latest-diff (:difference inp))) most-narrow)]

    (if less-than-any-narrow?

      ;; entry signal -> close is outside of band, and previous swing high/low is inside the band
      (if upM?

        (if (and (< (:last-trade-price (last ech-list)) (:upper-band (last ech-list)))
                 (> (:last-trade-price (last peaks)) (:upper-band (last (some #(= (:last-trade-time %)
                                                                                    (:last-trade-time (last peaks)))
                                                                                ech-list)))))

          (conj rslt (assoc (first ech-list) :signals [{:signal :down
                                                        :why :bollinger-close-abouve
                                                        :arguments [ech-list peaks]}]))

          (conj rslt (first ech-list)))

        (if (and (> (:last-trade-price (last ech-list)) (:lower-band (last ech-list)))
                 (< (:last-trade-price (last valleys))
                    (:lower-band (last (some #(= (:last-trade-time %)
                                                 (:last-trade-time (last valleys)))
                                             ech-list)))))

          (conj rslt (assoc (last ech-list) :signals [{:signal :up
                                                       :why :bollinger-close-below
                                                       :arguments [ech-list valleys]}]))

          (conj rslt (last ech-list)))))))

(defn calculate-strategy-b [rslt ech-list most-wide peaks valleys]

  (let [latest-diff (- (:upper-band (last ech-list)) (:lower-band (last ech-list)))
        more-than-any-wide? (some (fn [inp] (> latest-diff (:difference inp))) most-wide)]

    (if more-than-any-wide?

      ;; B iii RSI Divergence
      (let [
            OVER_BOUGHT 80
            OVER_SOLD 20
            rsi-list (relative-strength-index 14 ech-list)


            ;; i. price makes a higher high and
            higher-highPRICE? (if (empty? peaks)
                                false
                                (> (:last-trade-price (last ech-list))
                                   (:last-trade-price (last peaks))))


            ;; ii. rsi divergence makes a lower high
            lower-highRSI? (if (or (empty? peaks)
                                   (some #(nil? (:last-trade-time %)) rsi-list)
                                   #_ (not (nil? rsi-list)))
                             false
                             (< (:rsi (last rsi-list))
                                (:rsi (last (filter (fn [inp]
                                                      (= (:last-trade-time inp)
                                                         (:last-trade-time (last peaks))))
                                                    rsi-list)))))

            ;; iii. and divergence should happen abouve the overbought line
            divergence-overbought? (> (:rsi (last rsi-list))
                                      OVER_BOUGHT)



            ;; i. price makes a lower low
            lower-lowPRICE? (if (or (empty? valleys)
                                    (some #(nil? (:last-trade-time %)) rsi-list))
                              false
                              (< (:last-trade-price (last ech-list))
                                 (:last-trade-price (last valleys))))

            higher-highRSI? (if (or (empty? valleys)
                                    (not (nil? rsi-list)))
                              false
                              (> (:rsi (last rsi-list))
                                 (:rsi (last (filter (fn [inp]
                                                       (= (:last-trade-time inp)
                                                          (:last-trade-time (last valleys))))
                                                     rsi-list)))))

            divergence-oversold? (< (:rsi (last rsi-list))
                                    OVER_SOLD)]

        (if (and higher-highPRICE? lower-highRSI? divergence-overbought?)

          (conj rslt (assoc (last ech-list) :signals [{:signal :down
                                                       :why :bollinger-divergence-overbought
                                                       :arguments [peaks ech-list rsi-list]}]))

          (if (and lower-lowPRICE? higher-highRSI? divergence-oversold?)

            (conj rslt (assoc (last ech-list) :signals [{:signal :up
                                                         :why :bollinger-divergence-oversold
                                                         :arguments [valleys ech-list rsi-list]}]))

            (conj rslt (last ech-list)))))

      (conj rslt (last ech-list)))))


(defn bollinger-band-signals

  "Implementing signals for analysis/bollinger-band. Taken from these videos:
     i. http://www.youtube.com/watch?v=tkwUOUZQZ3s
     ii. http://www.youtube.com/watch?v=7PY4XxQWVfM

   A. when the band width is very low, can indicate that price will breakout sooner than later;
     i. MA is in an UP or DOWN market
     ii. check for narrow bollinger band width ; less than the most previous narrow band width
     iii. close is outside of band, and previous swing high/low is inside the band

   B. when the band width is very high (high volatility); can mean that the trend is ending soon; can i. change direction or ii. consolidate
     i. MA is in a sideways (choppy) market -> check if many closes that are abouve or below the bollinger band
     ii. check for a wide bollinger band width ; greater than the most previous wide band
     iii. RSI Divergence; i. price makes a higher high and ii. rsi devergence makes a lower high iii. and divergence should happen abouve the overbought line
     iv. entry signal -> check if one of next 3 closes are underneath the priors (or are in the opposite direction)"
  ([tick-window tick-list]
   (let [sma-list (simple-moving-average nil tick-window tick-list)
         bband (bollinger-band tick-window tick-list sma-list)]

     (bollinger-band-signals tick-window tick-list sma-list bband)))

  ([tick-window tick-list sma-list bband]

   (last (reductions (fn [rslt ech-list]

                       (let [
                             ;; track widest & narrowest band over the last 'n' ( 3 ) ticks
                             sorted-bands (sort-bollinger-band ech-list)
                             most-narrow (take 3 sorted-bands)
                             most-wide (take-last 3 sorted-bands)


                             partitioned-list (partition 2 1 (remove nil? ech-list))

                             upM? (up-market? 10 (remove nil? partitioned-list))
                             downM? (down-market? 10 (remove nil? partitioned-list))

                             ;; ... TODO - determine how far back to look (defaults to 10 ticks) to decide on an UP or DOWN market
                             ;; ... TODO - does tick price fluctuate abouve and below the MA
                             ;; ... TODO - B iv. entry signal -> check if one of next 3 closes are underneath the priors (or are in the opposite direction)
                             side-market? (if (and (not upM?)
                                                   (not downM?))
                                            true
                                            false)

                             ;; find last 3 peaks and valleys
                             peaks-valleys (find-peaks-valleys nil (remove nil? ech-list))
                             peaks (:peak (group-by :signal peaks-valleys))
                             valleys (:valley (group-by :signal peaks-valleys))]


                         (if (empty? (remove nil? ech-list))

                           (conj rslt nil)

                           (if (or upM? downM?)

                             ;; A.
                             (calculate-strategy-a rslt ech-list most-narrow upM? peaks valleys)

                             ;; B.
                             (calculate-strategy-b rslt ech-list most-wide peaks valleys)))

                         (conj rslt (first ech-list))))
                     []
                     (partition tick-window 1 bband)))))

(comment

  ;; 1.
  (def price-list (datasource/generate-prices))
  (def time-series (core/generate-timeseries price-list))

  (def prices (take 320 time-series))
  (def remaining (drop 320 time-series))

  ;; 2.
  (def sma (simple-moving-average {} 20 prices))
  (def ema (exponential-moving-average {} 20 prices))
  (def bol (bollinger-band 20 prices))

  (count (join-averages 20 (take 100 time-series)))

  (moving-averages-signals 20 (take 100 time-series))

  (relative-strength-index 14 (take 100 time-series))

  (bollinger-band-signals 20 (take 100 time-series))

  )
