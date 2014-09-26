 (ns initial.core)

(defn plus
 "Adds up numbers."
  [& xx]  (apply + xx))

(plus 5 10)
(plus 0)
(plus -1 2)
(plus 10 20 30 40 50)

(defn socialist-plus
  "Adds up numbers [x_1, x_2, ... x_n].  If n>2, it taxes the result by subtracting
  (n-2).  If n<2 it adds one as a subsidy."
  [& xx]
  (let [n (count xx)]
    (cond (< n 2) (apply + (cons 1 xx))
          (> n 2) (* -1 (apply - (cons (- n 2) xx)))
          (= n 2) (apply + xx)
    )))

(socialist-plus 10 20 30)
(socialist-plus 10 20 20 10)
(socialist-plus 10)
(socialist-plus 20 30)
; (socialist-plus 10 20 30) => 59
; (socialist-plus 10 20 20 10) => 58

(defn capitalist-plus
 "Adds up numbers [x_1, x_2, ... x_n].  If n>2, it subsidizes the result by adding
  (n-2).  If n<2 it subtracts one as a tax."
  [& xx]
  (let [n (count xx) ]
    (cond (< n 2) (* -1 (apply - (cons 1 xx)))
          (> n 2) (apply + (cons (- n 2) xx))
          (= n 2) (apply + xx))))

(capitalist-plus 10 20 30)
(capitalist-plus 10 20 20 10 )
(capitalist-plus 10)
(capitalist-plus 10 20)

; (capitalist-plus 10 20 30) => 61
; (capitalist-plus 10 20 20 10) => 62

(defn communist-plus
 ;"Adds up numbers.  To allow for equality the sum is always 10."
  ; I think you'll nail this one.
  [& xx]
  10
)

(communist-plus 20 10)
(communist-plus -40 50)

(defn political-extreemist-plus
 ; "Adds up numbers like a political extreemist, i.e., by multiplying them.
  ;You get to pick which political extreemists this refers to."
  [& xx]
  (apply * xx))

(political-extreemist-plus 20 10)
(political-extreemist-plus 20 30)
