(ns edgaru.one.core)


(+ 9 5)  ;; 14
(- 400 18.75)  ;; 381.25
(* 6 10)  ;; 60
(/ 50 5)  ;; 10
(quot 50 5)  ;; 10 ; the quotient (same as division `/`) of the numerator and denominator, respectively
(rem 26 12)  ;; 2 ; the remainder of the numerator and denominator, respectively
(mod 26 12)  ;; 2 ; the modulus (same as remainder `rem`) of the numerator and denominator, respectively
(inc 67)  ;; 68 ; increment the argument (a number) by 1
(dec 100)  ;; 99 ; decrement the argument (a number) by 1
(max 250 45)  ;; 250 ; the maximum number between the first and second arguments
(min 250 45)  ;; 45 ; the minimum number between the first and second arguments


(reverse "hello")  ;; (\o \l \l \e \h)
(count "hello")  ;; 5
(first "hello")  ;; \h


(require '[clojure.string :as s])

(s/reverse "hello")  ;; "olleh"
(reverse "hello")  ;; (\o \l \l \e \h)
(s/join ", " "hello") ;; "h, e, l, l, o"


(re-find #"\d+" "hello1234")  ;; "1234"
(s/replace "Hello World" #"World" "Clojure")  ;; "Hello Clojure"


(symbol 'hello)  ;; hello
(symbol "hello")  ;; hello
(symbol "clojure.core" "hello")  ;; clojure.core/hello
(def hello "world")  ;; #'user/hello
hello  ;; "world"


:hello  ;; :hello
(:foo {:foo "bar" :hello "world"})  ;; "bar"


(conj '("a" "s" "d" "f") "z")  ;; ("z" "a" "s" "d" "f") "z" gets "conjoined" to the front of the list, due to list's head first efficiency attribute
(count '("a" "s" "d" "f"))  ;; 4
(empty '("a" "s" "d" "f"))  ;; ()
(empty? '("a" "s" "d" "f"))  ;; false


(conj ["a" "s" "d" "f"] "z")  ;; ["a" "s" "d" "f" "z"] Here, "z" get "conjoined" to the end of the vector, as vectors offer efficient random access.
(mapv inc [1 2 3 4])  ;; [2 3 4 5] This applies (or maps) the `inc` function over each of the vector's elements


(#{"a" "s" "d" "f"} 2)  ;; nil
(#{"a" "s" "d" "f"} "d")  ;; "d"


({:hello "world" :foo "bar"} :foo)  ;; "bar"
(assoc {} :a "b")  ;; {:a "b"}
(zipmap [:hello :foo] ["world" "bar"])  ;; {:foo "bar", :hello "world"}
