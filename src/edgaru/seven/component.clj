(ns edgaru.seven.component
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async :refer [go go-loop chan close! <! >!]]
            [com.stuartsierra.component :as component]
            [clj-time.format :as fmt]
            [edgaru.seven.core :as core]
            [edgaru.seven.analytics :as analytics]
            [edgaru.seven.datasource :as datasource])
  (:import [java.text SimpleDateFormat]))



(defn generate-file-name [fname]
  (str "data/" fname))

(defn write-data [fname data]
  (let [full-path (generate-file-name fname)
        _ (io/make-parents full-path)]

    (binding [*print-length* nil]
      (spit full-path (pr-str data)
            :encoding "UTF-8"))))


;; =====
;; Component code

(defn send-data! [channel time-series]

  (go-loop [prices (take 320 time-series)
            remaining (drop 320 time-series)]

    (let [sma (analytics/simple-moving-average {} 20 prices)
          ema (analytics/exponential-moving-average {} 20 prices sma)
          bol (analytics/bollinger-band 20 prices sma)]

      (>! channel {:ticks prices :sma sma :ema ema :bol bol})
      (Thread/sleep 1000))

    (recur (take 320 remaining)
           (drop 320 remaining))))

(defn receive-data! [channel]

  (go-loop [data (<! channel)]

    (if-not (nil? data)
      (do
        (let [{ticks :ticks sma :sma ema :ema bol :bol} data
              dateformat (SimpleDateFormat. "MM-dd-yyy-hh:mm:ss")
              timestamp (->> ticks last :last-trade-time (.format dateformat))
              generate-file-name-with-timestamp-fn (fn [fname] (str timestamp "-" fname))]

          (write-data (generate-file-name-with-timestamp-fn "ticks.edn") ticks)
          (write-data (generate-file-name-with-timestamp-fn "sma.edn") sma)
          (write-data (generate-file-name-with-timestamp-fn "ema.edn") ema)
          (write-data (generate-file-name-with-timestamp-fn "bol.edn") bol))
        (recur (<! channel))))))

(defrecord Timeseries []
  component/Lifecycle

  (start [component]
    (let [c (chan)
          price-list (datasource/generate-prices)
          time-series (core/generate-timeseries price-list)]

      (send-data! c time-series)
      (assoc component :channel c)))

  (stop [component]
    (let [c (:channel component)]
      (close! c)
      (dissoc component :channel))))

(defrecord Consumer [timeseries]
  component/Lifecycle

  (start [component]
    (let [channel (:channel timeseries)]

      (receive-data! channel)
      (assoc component :channel channel)))

  (stop [component]
    (dissoc component :channel)))


;; ====
(defn new-timeseries []
  (map->Timeseries {}))

(defn new-consumer []
  (map->Consumer {}))


;; ====
(defn build-system []
  (component/system-map
   :tms (new-timeseries)
   :cns (component/using
              (new-consumer)
              {:timeseries :tms})))

(def system (build-system))


;; 1. call datasource/generate-prices (pull 320)

;; 2. call
;;   analytics/simple-moving-average
;;   analytics/exponential-moving-average
;;   analytics/bollinger-band

;; 3. save all batches to file (latest time increment .edn)

;; 4. put generate prices into core.async channel
;; 5. analytics reads data from channel

;; 6. wrap both ends into Components


(comment

  (with-open [wr (io/writer "myfile.txt")]
    (.write wr (str '(:new :data))))

  (def os (clojure.java.io/output-stream "myfile.txt"))
  (.write os 1)
  (.flush os)

  (write-data "f2" '(:some :thing))


  (reduce (fn [a v]
            (if (< a 100)
              (+ a v)
              (reduced :big)))
          (range 10))

  (reduce (fn [[prev acc] i]
            (if (> i 10)
              (reduced acc)
              [i (+ prev i)]))
          [0 0]
          (range))


  ;; 1.
  (def price-list (datasource/generate-prices))
  (def time-series (core/generate-timeseries price-list))

  (def prices (take 320 time-series))
  (def remaining (drop 320 time-series))

  ;; 2.
  (def sma (analytics/simple-moving-average {} 20 prices))
  (def ema (analytics/exponential-moving-average {} 20 prices sma))
  (def bol (analytics/bollinger-band 20 prices sma))

  ;; 3.
  (write-data "ticks.edn" prices)
  (write-data "sma.edn" sma)
  (write-data "ema.edn" ema)
  (write-data "bol.edn" bol)

  (let [c (chan)]

    ;; 4.
    (go (>! c {:ticks prices}))
    (go (>! c {:sma sma}))
    (go (>! c {:ema ema}))
    (go (>! c {:bol bol}))

    ;; 5.
    (println (<!! (go (<! c))))
    (println (<!! (go (<! c))))
    (println (<!! (go (<! c))))
    (println (<!! (go (<! c))))

    (close! c))


  ;; 6.
  (alter-var-root #'system component/start)

  (alter-var-root #'system component/stop)

)
