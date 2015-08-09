(ns edgaru.seven
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async :refer [go <! >!]]
            [com.stuartsierra.component :as component]
            [edgaru.two :as two]
            [edgaru.four :as four]
            [edgaru.five :as five]))


(defn generate-file-name [fname]
  (str "data/" fname))

(defn write-data [fname data]
  (let [full-path (generate-file-name fname)
        _ (io/make-parents full-path)]

    (spit full-path (list (apply pr-str data))
           :encoding "UTF-8")))

(defrecord Timeseries []
  component/Lifecycle

  (start [component]
    component)

  (stop [component]
    component))

(defrecord Consumer [channel]
  component/Lifecycle

  (start [component]
    component)

  (stop [component]
    component))

(defn new-timeseries []
  (map->Timeseries))

(defn new-consumer [channel]
  (map->Consumer {:channel channel}))


;; 1. call five/generate-prices (pull 320)

;; 2. call
;;   four/simple-moving-average
;;   four/exponential-moving-average
;;   four/bollinger-band

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


  ;; 1.
  (def price-list (five/generate-prices))
  (def time-series (two/generate-timeseries price-list))

  (def prices (take 320 time-series))
  (def remaining (drop 320 time-series))

  ;; 2.
  (def sma (four/simple-moving-average {} 20 prices))
  (def ema (four/exponential-moving-average {} 20 prices sma))
  (def bol (four/bollinger-band 20 prices sma))

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


  )
