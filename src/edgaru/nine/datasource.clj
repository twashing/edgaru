(ns edgaru.nine.datasource
  (:require [edgaru.nine.core :as core]
            [edgaru.nine.analytics :as analytics]
            [clojure.math.numeric-tower :as math])
  (:import [org.apache.commons.math3.distribution BetaDistribution]))


;; ==>
(defn polynomial [a b c x]
  (->
   (+ (* a
         (Math/pow x 3))

      (* b
         (Math/pow x 2)))
   (- (* c x))))

(defn polynomial-xintercept [x]
  (polynomial 2 2 3 x))


;; ==>
;; f(x) = a sin (b(x − c)) + d
;; y = a sin (b(x − c)) + d
;; y = a sin (b(x − pi/2)) + d

;; d - quantifies vertical translation
;; a - amplitude of the wave
;; b - horizontal dilation
;; c - horizontal translation
(defn sine [a b d x]
  (- (* a
        (Math/sin (* b
                     (- x
                        (/ Math/PI 2)))))
     d))

(defn sine-xintercept [x]
  (sine 2 2 0 x))


;; ==>
(defn ydirection [ypos]
  (if (pos? ypos)
    :positive
    :negative))

(defn direction-changed? [ypos dirn]
  (not (= (ydirection ypos)
          dirn)))

(defn get-opposite-direction-key [ydir]
  (if (= ydir :positive)
    :negative
    :positive))

(defn get-opposite-direction-fn [dirn]
  (if (= dirn +) - +))

(defn effectively-zero? [xval]
  (= 0.0 (Double. (format "%.7f" xval))))

(defn find-xintercept [direction mfn]

  ;; find initial direction of y (take y at 0.1 of x=0)
  ;; step 1 in given direction until we pass through x
  (loop [start-point 0.0
         distance    1.0
         ydir        (ydirection (mfn (direction 0 0.1)))
         dirn        direction]

    (let [next-point (dirn start-point distance)]

      (if (effectively-zero? (mfn next-point))

        next-point

        (let [dc? (direction-changed? (mfn next-point) ydir)]

          ;; (println (str "start[" start-point "] / end[" next-point "] / direction-changed?[" dc? "] / Y[" (mfn next-point) "]"))
          ;; divide that step in half and go the other direction, until we again pass through x
          ;; repeat until we have a zero up to the 7th digit
          (recur next-point
                 (if dc? (/ distance 2) distance)
                 (if dc? (get-opposite-direction-key ydir) ydir)
                 (if dc? (get-opposite-direction-fn dirn) dirn)))))))


;; phases - figuring out x intercepts
;; repeatable
;;   mark phase start / end
;; => randomize phase entry point
;; => randomize sample length of phase (between 50% - 100%)

(comment

  ;; find x-intercepts both left and right from x=0.
  (find-xintercept - polynomial-xintercept)
  -1.8228756561875343

  (find-xintercept + polynomial-xintercept)
  0.8228756487369537

  (find-xintercept - sine-xintercept)
  -1.570796325802803

  (find-xintercept + sine-xintercept)
  1.570796325802803

  )

;; stolen from lazytest - no longer under active development
;; https://github.com/stuartsierra/lazytest/blob/master/modules/lazytest/src/main/clojure/lazytest/random.clj
(defn rand-double-in-range
  "Returns a random double between min and max."
  [min max]
  {:pre [(<= min max)]}
  (+ min (* (- max min) (Math/random))))


;; vertical dilation
;; => randomize vertical dilation (or amplitude)
;;   polynomial: 2 - 0.5
;;   sine: 0.5 - 2.7

(defn randomize-vertical-dilation-P [x]  ;; [x mfn [min' max']]
  (let [a (rand-double-in-range 0.5 2)]
    (polynomial a 2 3 x)))

(defn randomize-vertical-dilation-S [x]
  (let [a (rand-double-in-range 0.5 2.7)]
    (sine a 2 0 x)))


(defn randomize-vertical-dilation [mathfn min' max']
  (let [a (rand-double-in-range min' max')]
    (partial mathfn a)))


;; horizontal dilation
;; => randomize horizontal dilation
;;   polynomial: between 2 - 0.5 (larger b yields a narrower curve)
;;   sine: 2.7 - 0.3

(defn randomize-horizontal-dilation [mathfn-curried min' max']
  (let [b (rand-double-in-range min' max')]
    (partial mathfn-curried b)))


;; granularity
;; => randomize granularity, or zoom of sample
;;   polynomial:  between 0.1 - 1
;;   sine:  between 0.1 - 1


;; combination
;;   (phase begin / end)
;;   (beta curve) Sine + Polynomial + Stochastic Oscillating, distributed under a Beta Curve
;; => beta distribution of a=2 b=4.1 x=0 (see: http://keisan.casio.com/exec/system/1180573226)

(defn test-beta [beta-distribution]
  (let [sample-val (.sample beta-distribution)]
    (cond
     (< sample-val 0.33) :a
     (< sample-val 0.66) :b
     :else :c)))

(defn generate-polynomial-sequence []

  (def one (randomize-vertical-dilation polynomial 0.5 2))
  (def two (randomize-horizontal-dilation one 0.5 2))
  (def polyn-partial (partial two 3))

  (def xinterc-polyn-left (find-xintercept - polynomial-xintercept))
  (def xinterc-polyn-right (find-xintercept + polynomial-xintercept))


  (def granularityP (rand-double-in-range 0.1 1))
  (def xsequenceP (iterate (partial + granularityP) xinterc-polyn-left))

  (map polyn-partial xsequenceP))

(defn generate-sine-sequence []

  (def ein (randomize-vertical-dilation sine 0.5 2.7))
  (def zwei (randomize-horizontal-dilation ein 0.3 2.7))
  (def sine-partial (partial zwei 0))

  (def xinterc-sine-left (find-xintercept - sine-xintercept))
  (def xinterc-sine-right (find-xintercept + sine-xintercept))

  (def granularityS (rand-double-in-range 0.1 1))
  (def xsequenceS (iterate (partial + granularityS) xinterc-sine-left))

  (map sine-partial xsequenceS))

(defn generate-oscillating-sequence []

  (analytics/generate-prices-without-population 5 15))


(defn sample-dispatcher [sample-type sample-length sample-fn]

  (let [sample-seq (take sample-length (sample-fn))]

    ;;(println (str "Generating [" sample-type "] sample / length[" sample-length "]"))
    ;;(println sample-seq)
    ;;(println)

    sample-seq))

(defn sample-prices [beta-distribution]

  ;; [ok] have a sequence that iteratively calls the below sample `let`
  ;; [ok] randomize length of each sample

  ;; start-point
  ;; move next sequence to endpoint of previous
  ;; ... reduce / cat

  (let [sample-val (.sample beta-distribution)]

    (cond
     (< sample-val 0.33) (sample-dispatcher :sine (rand-double-in-range 10 15) generate-sine-sequence)
     (< sample-val 0.66) (sample-dispatcher :polynomial (rand-double-in-range 4 6) generate-polynomial-sequence)
     :else               (sample-dispatcher :oscillating (rand-double-in-range 8 10) generate-oscillating-sequence))))


(defn generate-prices-orig [beta-distribution]

  (reduce (fn [^clojure.lang.LazySeq rslt
              ^clojure.lang.LazySeq each-sample-seq]

            (let [beginning-price (if (empty? rslt)
                                    (rand-double-in-range 5 15)
                                    (last rslt))
                  sample-seq-head (first each-sample-seq)
                  price-difference (math/abs (- sample-seq-head beginning-price))]

              ;; only raise the price if below the beginning price
              (if (< sample-seq-head beginning-price)
                (concat rslt (map #(+ % price-difference) each-sample-seq))
                (concat rslt (map #(- % price-difference) each-sample-seq) each-sample-seq))))
          '()
          (repeatedly #(sample-prices beta-distribution))))


(defn generate-prices-iterate [beta-distribution]

  (let [sample-seq (repeatedly #(sample-prices beta-distribution))

        iterfn (fn [[^clojure.lang.LazySeq rslt
                    ^clojure.lang.LazySeq remaining-sample-seq]]

                 (let [each-sample-seq (first remaining-sample-seq)
                       beginning-price (if (empty? rslt)
                                         (rand-double-in-range 5 15)
                                         (last rslt))
                       sample-seq-head (first each-sample-seq)
                       price-difference (math/abs (- sample-seq-head beginning-price))]

                   ;; only raise the price if below the beginning price
                   (if (< sample-seq-head beginning-price)

                     [(concat rslt (map #(+ % price-difference) each-sample-seq))
                      (rest remaining-sample-seq)]
                     [(concat rslt (map #(- % price-difference) each-sample-seq))
                      (rest remaining-sample-seq)])))]

    (map first (iterate iterfn ['() sample-seq]))))


(defn generate-prices-for [beta-distribution]

  (def previous-price nil)

  (let [adjusted-samples (for [each-sample-seq (repeatedly #(sample-prices beta-distribution))
                                :let [beginning-price (if (nil? previous-price)
                                                        (rand-double-in-range 5 15)
                                                        previous-price)

                                      sample-seq-head (first each-sample-seq)
                                      price-difference (math/abs (- sample-seq-head beginning-price))

                                      adjusted-sample (if (< sample-seq-head beginning-price)
                                                        (map #(+ % price-difference) each-sample-seq)
                                                        (map #(- % price-difference) each-sample-seq))

                                      _ (alter-var-root #'previous-price (fn [x] (last adjusted-sample)))]]

                           adjusted-sample)]

    (apply concat adjusted-samples)))


(defn generate-prices-partition [beta-distribution]

  (let [samples-sequence (repeatedly #(sample-prices beta-distribution))
        partitioned-sequences (partition 2 1 samples-sequence)

        mapping-fn (fn [[fst snd]]

                     (let [beginning-price (last fst)
                           sample-seq-head (first snd)
                           price-difference (math/abs (- sample-seq-head beginning-price))]

                       (if (< sample-seq-head beginning-price)
                         (concat fst (map #(+ % price-difference) snd))
                         (concat fst (map #(- % price-difference) snd)))))]

    (apply concat (map mapping-fn partitioned-sequences))))


(defn generate-prices-reductions [beta-distribution]

  (reductions (fn [^clojure.lang.LazySeq rslt
                  ^clojure.lang.LazySeq each-sample-seq]

                (let [beginning-price (if (empty? rslt)
                                        (rand-double-in-range 5 15)
                                        (last rslt))
                      sample-seq-head (first each-sample-seq)
                      price-difference (math/abs (- sample-seq-head beginning-price))]

                  ;; only raise the price if below the beginning price
                  (if (< sample-seq-head beginning-price)
                    (concat rslt (map #(+ % price-difference) each-sample-seq))
                    (concat rslt (map #(- % price-difference) each-sample-seq) each-sample-seq))))
              '()
              (repeatedly #(sample-prices beta-distribution))))


(defn generate-prices
  ([] (generate-prices (BetaDistribution. 2.0 4.1)))
  ([beta-distribution]

   (map (fn [inp]
          (if (neg? inp)
            (* -1 inp)
            inp))
        (generate-prices-reductions beta-distribution))))


(defmethod print-method clojure.lang.PersistentQueue
  [q, w]
  (print-method '<- w) (print-method (seq q) w) (print-method '-< w))

(comment

  ;; (def bdist1 (BetaDistribution. 2.0 5.0))
  ;; (def bdist2 (BetaDistribution. 2.0 4.0))
  ;; (def bdist3 (BetaDistribution. 2.0 3.0))
  ;; (def bdist-even (BetaDistribution. 2.0 2.0))

  (def bdist (BetaDistribution. 2.0 4.1))
  (def result (repeatedly #(test-beta bdist)))
  (sort (take 100 result))

  (take 5 (generate-prices-orig bdist))

  (take 20 (generate-prices-iterate bdist))
  (take 2 (generate-prices-for bdist))
  (take 2 (generate-prices-partition bdist))

  (last (take 20 (generate-prices-reductions bdist)))

  )


;; Traversing Data
;; loop / recur (find-xintercept)

;; Branching and Conditional Dispatch
;;   case , cond , multi methods , pattern matching (core.match)

;; First Order Functions. http://christophermaier.name/blog/2011/07/07/writing-elegant-clojure-code-using-higher-order-functions
;; partial, apply, comp, juxt

;; [x] repeatedly -> reduce (not lazy)
;; [ok] repeatedly -> iterate (lazy)
;; Transducers ...

;; sorting, grouping, filtering, dequeing
;; Scalars: numbers & precision
