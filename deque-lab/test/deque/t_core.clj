(ns deque.t-core
  (:use midje.sweet)
  (:use [deque.core])
  (:import [deque.core Deque] ))

(facts "about deque-size"
       (let [a (Deque. '(1) '(2) 2)]
       (fact "returns size of deque."
             (deque-size a) => 2
       )
))


(facts "about push-front"
       (let [a (Deque. '(1) '(2) 2)]
       (fact "Adds an element to the front of the deque."
             (push-front a 1) => (Deque. '(1 1) '(2) 3)
       )
))



(facts "about push-front"
       (let [a (Deque. '(1) '(2) 2)]
       (fact "Adds an element to the back of the deque."
             (push-back a 1) => (Deque. '(1) '(1 2) 3)
       )
))



(ns deque.t-core
  (:use midje.sweet)
  (:use [deque.core])
  (:import [deque.core Deque] ))

(facts "about deque-size"
       (let [a (Deque. '(1) '(2) 2)]
       (fact "returns size of deque."
             (deque-size a) => 2
       )
))


(facts "about push-front"
       (let [a (Deque. '(1) '(2) 2)]
       (fact "Adds an element to the front of the deque."
             (push-front a 1) => (Deque. '(1 1) '(2) 3)
       )
))



(facts "about push-front"
       (let [a (Deque. '(1) '(2) 2)]
       (fact "Adds an element to the back of the deque."
             (push-back a 1) => (Deque. '(1) '(1 2) 3)
       )
))



(facts "about flip-front"
       (let [a (Deque. '(1) '(2) 2)
             b (Deque. '() '(1) 1)]
       (fact "Flip the back list to the front list, if necessary."
             (flip-front a) => (Deque. '(1) '(2) 2)
             (flip-front b) => (Deque. '(1) '() 1)
       )
))



(facts "about flip-back"
       (let [a (Deque. '(1) '(2) 2)
             b (Deque. '(1) '() 1)
             c (Deque. '() '(1 2 3) 3)]
       (fact "Flip the back list to the back list, if necessary."
             (flip-back a) => (Deque. '(1) '(2) 2)
             (flip-back b) => (Deque. '() '(1) 1)
       )
))

(facts "about front"
       (let [a (Deque. '(1) '(2) 2)
             b (Deque. '(1) '() 1)]
       (fact "Returns the front element of the deque.  May cause a flip.."
             (front a) => 1
             (front b) => 1
       )
))

(facts "about back"
       (let [a (Deque. '() '(1 2 3) 3)
             b (Deque. '(1 2 3) '() 3)
            ]
       (fact "Returns the back element of the deque.  May cause a flip.."
             (back a) => 1
       )
       (fact "It will cause a flip if back is empty"
             (back b) => 3)
))


(def z (Deque. '(1 2 3) '(1 2 3) 3 ))
(back z)

(facts "about pop-front"
       (let [a (Deque. '() '(1 2 3) 3)
             b (Deque. '(1 2 3) '() 3)
            ]
       (fact "Pops/dequeues an element from the front of the deque.."
             (pop-front a) => (Deque. '(2 1) '() 2)
             (pop-front b) => (Deque. '(2 3) '() 2)
       )
))





(facts "about pop-back"
       (let [a (Deque. '() '() 0)
             b (Deque. '() '(1 2 3) 3)
             c (Deque. '(1 2 3) '() 3)
            ]
       (fact "Pops/dequeues an element from the back of the deque.."
             (pop-back b) => (Deque. '() '(2 3) 2)
             (pop-back c) => (Deque. '() '(2 1) 2)
       )
       (fact "Function does not let the size of the deque become negative"
             (pop-back a) => (Deque. '() '() 0))
))
