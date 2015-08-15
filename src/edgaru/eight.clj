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
(slurp (second many-files))


;; 3. functions for querying a system
;;  lookup based on specific time
;;  lookup based on time range
;;  lookup based on specific price
;;  lookup based on price range


;; 4. simple query language backed by macro
;;   lookup system to be a logic language
