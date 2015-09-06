(ns edgaru.six.six)


(let [a (float 0.1)
      b (float 0.4)
      c (float 0.8)]
  (=
   (* a (+ b c))
   (+ (* a b) (* a c))))


(reduce (fn [a v]
          (if (< a 100)
            (+ a v)
            (reduced :big)))
        (range 20))


(reduce (fn [rslt ech]
          (if (< rslt 20)
            (+ rslt ech)
            (reduced :break)))
        [1 2 3 4 5 6 7 :break 8 9 10])

(defn update-map [m f]
  (reduce-kv (fn [m k v]
               (assoc m k (f v)))
             {}
             m))


(def xs [1 2 3])

(areduce xs i ret 0
         (+ ret (aget xs i)))
