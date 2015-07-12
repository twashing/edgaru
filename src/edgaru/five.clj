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
