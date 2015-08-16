(ns edgaru.eight
  (:require [clojure.java.io :as io]))


;; 0. with-open explanation

;; 1. read one file
;; slurp
(def one-file (slurp "data/08-15-2015-04:55:56-ticks.edn"))
(def one-edn (edn/read-string one-file))

;; [ok] problem with the written data -> string within a string

;; [ok] problems reading in JodaTime dates -> 2 options
;;   change your Clojure reader to use JodaTime as #inst Class
;;   refactor time-series function to coerce JodaTime to Java's default Date
;;   -> discuss engineering tradeoffs


;; 2. read all files
;; file-seq
(def many-files (file-seq (io/file "data/")))

;; slurp takes a file name or a file object
(def second-file (slurp (second many-files)))

;; we've now demonstrated roundtripping of our data
(def second-edn (edn/read-string second-file))


;; 3. functions for querying a system

;; regular expressions
(comment

  (re-matches #"x" "foobar")
  ;; nil

  (re-matches #"foo.*" "foobar")
  ;; "foobar"
  )

;; filter on the type of file
(filter #(re-matches #".*\-ticks.edn" (.getName %))
        many-files)


;; basic lookup
(comment

  (map (fn [x]
         (let [read-fn (comp edn/read-string slurp)
               pred-fn #(= #inst "2015-08-15T17:18:51.352-00:00" (:last-trade-time %))
               inp-edn (read-fn x)]

           (filter pred-fn
                   inp-edn)))

       (filter #(.isFile %) many-files))

  '(({:last-trade-time #inst "2015-08-15T17:18:51.352-00:00",
     :last-trade-price 101.90402553222992}
    {:last-trade-time #inst "2015-08-15T17:18:51.352-00:00",
     :last-trade-price 101.63143059175933})
   ()
   ()
   ()
   ()
   ()
   ()
   ()
   ()
   ()
   ()
   ()
   ()
   ()
   ()
   ()
   ()
   ()
   ()
   ())
  )


(comment

  (defn lookup []

    (flatten
     (map (fn [x]
            (let [read-fn (comp edn/read-string slurp)
                  pred-fn #(= #inst "2015-08-15T17:18:51.352-00:00" (:last-trade-time %))
                  inp-edn (read-fn x)]

              (filter pred-fn
                      inp-edn)))

          (filter #(.isFile %) many-files))))

  ;; ({:last-trade-time #inst "2015-08-15T17:18:51.352-00:00", :last-trade-price 101.90402553222992} {:last-trade-time #inst "2015-08-15T17:18:51.352-00:00", :last-trade-price 101.63143059175933})

  )


;;  lookup based on specific time
;;  lookup based on time range
;;  lookup based on specific price
;;  lookup based on price range

(comment

  (defn lookup [flist pred-fn]

    (flatten
     (map (fn [x]
            (let [read-fn (comp edn/read-string slurp)
                  inp-edn (read-fn x)]
              (filter pred-fn
                      inp-edn)))
          flist)))

  (defn specific-time-pred [inst]
    #(= inst (:last-trade-time %)))

  (defn time-range-pred [lower upper]
    #(and (.after (:last-trade-time %) lower)
          (.before (:last-trade-time %) upper)))

  (defn specific-price-pred [price]
    #(= price (:last-trade-price %)))

  (defn price-range-pred [lower upper]
    #(and (> (:last-trade-price %) lower)
          (< (:last-trade-price %) upper)))

  (def alist (filter #(.isFile %) many-files))


  (lookup alist (specific-time-pred #inst "2015-08-15T17:18:51.352-00:00"))

  (lookup alist (time-range-pred #inst "2015-08-15T17:18:00.000-00:00" #inst "2015-08-15T17:19:00.000-00:00"))

  (lookup alist (specific-price-pred 4.028309189176084))

  (lookup alist (price-range-pred 6 10))

  )


;; 4. simple query language backed by macro

;;   ? lookup system to be a logic language

;;   assume data/ directory

(comment

  (lookup :time "2015-08-15T17:18:51.352-00:00")
  (lookup :time-after "2015-08-15T17:18:00.000-00:00")
  (lookup :time-before "2015-08-15T17:19:00.000-00:00")
  (lookup :time-after "2015-08-15T17:18:00.000-00:00" :time-before "2015-08-15T17:19:00.000-00:00")

  (lookup :price 4.028309189176084)
  (lookup :price-abouve 12)
  (lookup :price-below 12)
  (lookup :price-abouve 12 :price-below 20)

  (lookup :time-after "2015-08-15T17:18:00.000-00:00"
          :time-before "2015-08-15T17:19:00.000-00:00"
          :price-abouve 12
          :price-below 20))
