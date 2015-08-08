(ns edgaru.seven
  (:require [clojure.java.io :as io]))


(defn generate-file-name [fname]
  (str "data/" fname))

(defn write-data [fname data]
  (let [full-path (generate-file-name fname)
        _ (io/make-parents full-path)]

    (spit full-path data
           :encoding "UTF-8"
           :append true)))

(comment


  (with-open [wr (io/writer "myfile.txt")]
    (.write wr (str '(:new :data))))

  (def os (clojure.java.io/output-stream "myfile.txt"))
  (.write os 1)
  (.flush os)

  (write-data "f2" '(:some :thing))

  )
