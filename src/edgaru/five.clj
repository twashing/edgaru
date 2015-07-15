(ns edgaru.five)


(defn polynomial [x]
  (->
   (+ (* 2
         (Math/pow x 3))

      (* 2
         (Math/pow x 2)))
   (- (* 3 x))))


;; f(x) = a sin (b(x − c)) + d
;; y = a sin (b(x − c)) + d
;; y = a sin (b(x − pi/2)) + d

;; d - quantifies vertical translation
;; a - amplitude of the wave
;; b - horizontal dilation
;; c - horizontal translation

(defn sine [a b x d]

  (- (* a
         (Math/sin (* b
                      (- x
                         (/ Math/PI 2)))))
     d))


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

;; find x-intercepts both left and right from x=0.
(defn find-xintercept [direction mfn]

  ;; find initial direction of y (take y at 0.1 of x=0)
  ;; step 1 in given direction until we pass through x
  (loop [start-point 0.0
         distance    1.0
         ydir        (ydirection (mfn (direction 0 0.1)))
         dirn        direction]

    ;; divide that step in half and go the other direction, until we again pass through x
    ;; repeat until we have a zero up to the 7th digit
    (let [next-point (dirn start-point distance)]

      (if (= 0.0 (Double. (format "%.7f" (mfn next-point))))
        next-point
        (let [dc? (direction-changed? (mfn next-point) ydir)]

          (println (str "start[" start-point "] / end[" next-point "] / direction-changed?[" dc? "] / Y[" (mfn next-point) "]"))

          (recur next-point
                 (if dc? (/ distance 2) distance)
                 (if dc? (get-opposite-direction-key ydir) ydir)
                 (if dc? (get-opposite-direction-fn dirn) dirn)))))))



;; loop / recur

;; phases - figuring out x intercepts
;; repeatable
;;   mark phase start / end

;; vertical dilation
;; horizontal dilation
;; granularity
;; combination
;;   (phase begin / end)
;;   (beta curve) Sine + Polynomial + Stochastic Oscillating, distributed under a Beta Curve


;; randomize phase entry point
;; randomize sample length of phase (between 50% - 100%)
;; randomize vertical dilation (or amplitude, between 0.5 - 2)
;; randomize horizontal dilation (between 2 - 0.5 (larger b yields a narrower curve))
;; randomize granularity, or zoom of sample (between 0.1 - 1)
;; beta distribution of a=2 b=4 x=0 (see: http://keisan.casio.com/exec/system/1180573226)


;; Traversing Data
;; Branching and Conditional Dispatch
;;   case , cond , multi methods , pattern matching (core.match)
;; sorting, grouping, filtering, dequeing
;; Scalars: numbers & precision
