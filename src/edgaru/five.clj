(ns edgaru.five)


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


(comment

  (def one (randomize-vertical-dilation polynomial 0.5 2))
  (def two (randomize-horizontal-dilation one 0.5 2))
  (def polyn-partial (partial two 3))


  (def xinterc-left (find-xintercept - polynomial-xintercept))
  -1.8228756561875343

  (def xinterc-right (find-xintercept + polynomial-xintercept))
  0.8228756487369537

  (def granularity (rand-double-in-range 0.1 1))
  (def xsequence (iterate (partial + granularity) xinterc-left))

  ;; FINAL - these are our price values (y @ respective x granularity)
  (map polyn-partial (take 10 xsequence))

  )

;; granularity
;; => randomize granularity, or zoom of sample
;;   polynomial:  between 0.1 - 1
;;   sine:  between 0.1 - 1


;; combination
;;   (phase begin / end)
;;   (beta curve) Sine + Polynomial + Stochastic Oscillating, distributed under a Beta Curve
;; => beta distribution of a=2 b=4 x=0 (see: http://keisan.casio.com/exec/system/1180573226)




;; Traversing Data
;; loop / recur (find-xintercept)

;; Branching and Conditional Dispatch
;;   case , cond , multi methods , pattern matching (core.match)
;; sorting, grouping, filtering, dequeing
;; Scalars: numbers & precision
