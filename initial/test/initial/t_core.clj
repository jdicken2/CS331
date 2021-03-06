(ns initial.t-core
  (:use midje.sweet)
  (:use [initial.core]))

(facts "about plus"
  (fact "it adds numbers."
        (plus) => 0
        (plus 10) => 10
        (plus 10 20) => 30
        (plus 10 20 30 40 50) => 150))


(facts "about socialist plus"
       (fact "it subsidized fewer than two elements"
        (socialist-plus 10) => 11)
       (fact "it does nothing with two elements. Except add."
        (socialist-plus 20 30) => 50)
       (fact "it taxes the result if there are more than 2 elements."
        (socialist-plus 10 20 30) => 59))



(facts "about capitalist plus"
       (fact "it taxes fewer than two elements"
             (capitalist-plus 10)=> 9)
       (fact "it does nothing with two elements. Except add."
             (capitalist-plus 20 30)=> 50)
       (fact "it subsidizes the result if there are more than 2 elements."
             (capitalist-plus 10 20 30)=> 61))

(facts "about communist plus"
  (fact "it only returns 10."
    (communist-plus 20 30)=> 10))

(facts "about political extreemist plus"
  (fact "it multiplies instead of adds."
        (political-extreemist-plus 20 30)=> 600 )
        (political-extreemist-plus)=> 1
        (political-extreemist-plus 10 20 30)=> 6000
        (political-extreemist-plus 5 4 6 7)=> 840)

(facts "about this lab"
       true)
