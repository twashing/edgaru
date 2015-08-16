(ns edgaru.eight
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))


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
         (let [read-fn (comp edn/read-string slurp)   ;; -> use of `comp` function
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

  (defn lookupfn []

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

(defn lookupfn [flist pred-fn]

    (flatten
     (map (fn [x]
            (let [read-fn (comp edn/read-string slurp)
                  inp-edn (read-fn x)]
              (filter pred-fn
                      inp-edn)))
          flist)))

(comment

  (defn specific-time-pred [inst]   ;; -> functions returning functions
    #(= inst (:last-trade-time %)))

  (defn time-after-pred [time]
    #(.after (:last-trade-time %) time))

  (defn time-before-pred [time]
    #(.before (:last-trade-time %) time))

  (defn time-range-pred [lower upper]
    #(and (.after (:last-trade-time %) lower)
          (.before (:last-trade-time %) upper)))

  (defn specific-price-pred [price]
    #(= price (:last-trade-price %)))

  (defn price-abouve-pred [price]
    #(> (:last-trade-price %) price))

  (defn price-below-pred [price]
    #(< (:last-trade-price %) price))

  (defn price-range-pred [lower upper]
    #(and (> (:last-trade-price %) lower)
          (< (:last-trade-price %) upper)))

  (def alist (filter #(.isFile %) many-files))


  (lookupfn alist (specific-time-pred #inst "2015-08-15T17:18:51.352-00:00"))

  (lookupfn alist (time-range-pred #inst "2015-08-15T17:18:00.000-00:00" #inst "2015-08-15T17:19:00.000-00:00"))

  (lookupfn alist (specific-price-pred 4.028309189176084))

  (lookupfn alist (price-range-pred 6 10))

  )


;; 4. simple query language backed by macro

(defn load-data-files [fname]
  (filter #(.isFile %) (file-seq (io/file fname))))

(defn lookup [& constraints]

  ;; ensure constraints are in pairs -> Preconditions
  {:pre [(even? (count constraints))]}

  ;; map over pairs - find predicate fn based on keyword - partially apply fn with arg
  (let [alist (if (some #{:source} constraints)

                (let [source-source (comp (partial filter #(= :source (first %1)))    ;; -> a bigger use of `comp` function
                                          (partial partition 2))
                      source-value (comp second source-source)
                      source-key (comp first source-source)]

                  (if (string? source-key)
                    (load-data-files (source-key constraints))
                    source-value))

                (load-data-files "data/"))

        constraint-pairs (->> constraints
                              (partition 2)
                              (remove #(= :source (first %))))

        find-lookup-fn (fn [inp]
                         (case inp
                           :time specific-time-pred
                           :time-after time-after-pred
                           :time-before time-before-pred
                           :price specific-price-pred
                           :price-abouve price-abouve-pred
                           :price-below price-below-pred))

        constraint-pairs-A (map (fn [x]
                                  [(find-lookup-fn (first x)) (second x)])
                                constraint-pairs)

        lookupfn-fns-A (map (fn [x]
                                  (fn [y]
                                    (lookupfn y ((first x) (second x)))))
                            constraint-pairs-A)]

    ;; apply all fns with args
    (apply concat ((apply juxt lookupfn-fns-A)
                   alist))))


;; acts as an implicit "or"
;; later - negation

(comment

  (def many-files (file-seq (io/file "data/")))

  (lookup :time #inst "2015-08-15T17:18:51.352-00:00")
  (lookup :time-after #inst "2015-08-15T17:18:00.000-00:00")
  (lookup :time-before #inst "2015-08-15T17:19:00.000-00:00")
  (lookup :time-after #inst "2015-08-15T17:18:00.000-00:00" :time-before #inst "2015-08-15T17:19:00.000-00:00")

  (lookup :price 4.028309189176084)
  (lookup :price-abouve 12)
  (lookup :price-below 12)
  (lookup :price-abouve 12 :price-below 20)

  (lookup :time-after #inst "2015-08-15T17:18:00.000-00:00"
          :time-before #inst "2015-08-15T17:19:00.000-00:00"
          :price-abouve 12
          :price-below 20)

  (lookup :source many-files
          :time #inst "2015-08-15T17:18:51.352-00:00")

  (lookup :source "data/"
          :time #inst "2015-08-15T17:18:51.352-00:00")
  )


(defn generate-input-list [constraints]
  (if (some #{:source} constraints)
    (let [source-source (comp (partial filter #(= :source (first %1)))    ;; -> a bigger use of `comp` function
                              (partial partition 2))
          source-value (comp second source-source)
          source-key (comp first source-source)]

      (if (string? source-key)
        (load-data-files (source-key constraints))
        source-value))

    (load-data-files "data/")))

(defn generate-constraint-pairs [constraints]
  (->> constraints
       (partition 2)
       (remove #(= :source (first %)))))

(defn find-lookup-fn [inp]
  (case inp
    :time specific-time-pred
    :time-after time-after-pred
    :time-before time-before-pred
    :price specific-price-pred
    :price-abouve price-abouve-pred
    :price-below price-below-pred))

(defn lookup-refactored [& constraints]

  ;; ensure constraints are in pairs -> Preconditions
  {:pre [(even? (count constraints))]}

  ;; map over pairs - find predicate fn based on keyword - partially apply fn with arg
  (let [alist (generate-input-list constraints)
        constraint-pairs (generate-constraint-pairs constraints)

        constraint-pairs-A (map (fn [x]
                                  [(find-lookup-fn (first x)) (second x)])
                                constraint-pairs)

        lookupfn-fns-A (map (fn [x]
                                  (fn [y]
                                    (lookupfn y ((first x) (second x)))))
                            constraint-pairs-A)]

    ;; apply all fns with args
    (apply concat ((apply juxt lookupfn-fns-A)
                   alist))))

#_(defn lookup [& constraints]

  ;; ensure constraints are in pairs -> Preconditions
  {:pre [(even? (count constraints))]}

  ;; map over pairs - find predicate fn based on keyword - partially apply fn with arg
  (let [alist (if (some #{:source} constraints)

                (let [source-source (comp (partial filter #(= :source (first %1)))    ;; -> a bigger use of `comp` function
                                          (partial partition 2))
                      source-value (comp second source-source)
                      source-key (comp first source-source)]

                  (if (string? source-key)
                    (load-data-files (source-key constraints))
                    source-value))

                (load-data-files "data/"))

        constraint-pairs (->> constraints
                              (partition 2)
                              (remove #(= :source (first %))))

        find-lookup-fn (fn [inp]
                         (case inp
                           :time specific-time-pred
                           :time-after time-after-pred
                           :time-before time-before-pred
                           :price specific-price-pred
                           :price-abouve price-abouve-pred
                           :price-below price-below-pred))

        constraint-pairs-A (map (fn [x]
                                  [(find-lookup-fn (first x)) (second x)])
                                constraint-pairs)

        lookupfn-fns-A (map (fn [x]
                                  (fn [y]
                                    (lookupfn y ((first x) (second x)))))
                            constraint-pairs-A)

        ;; apply all fns with args
        rone (apply concat ((apply juxt lookupfn-fns-A)
                            alist))


        xxx (map (fn [x]
                   ((first x) (second x)))
                 constraint-pairs-A)


        lookupfn
        xxx
        alist

        yyy (map (fn [xfn]
                   (lookupfn ) (xfn))
                 alist)

        ;; (f1 x) (f2 x)

        _ (reduce (fn [rs efn]
                    (and rs (lookup efn)))
                  true
                  xxx)


        zzz (apply and yyy)

        rtwo (filter zzz alist)]

    rone))
