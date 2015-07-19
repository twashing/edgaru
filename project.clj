(defproject edgaru "0.1.0-SNAPSHOT"
  :description ""
  :url "https://github.com/twashing/edgaru"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-time "0.9.0"]
                 [com.taoensso/timbre "3.4.0"]
                 [overtone "0.9.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.apache.commons/commons-math3 "3.5"]]
  :profiles {:dev {:dependencies [[alembic "0.3.2"]]}})
