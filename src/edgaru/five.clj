(ns edgaru.five)


  (defn polynomial [x]
    (->
     (+ (* 2
           (Math/pow x 3))

        (* 2
           (Math/pow x 2)))
     (- (* 3 x))))

(comment


  )
