(ns deque.core)

(defrecord Deque [front back size])

;; # Your Work

(defn make-deque
  "Create an empty deque."
  []
  (Deque. '() '() 0))

(defn deque-size
  "Return the size of a deque."
  [dq]
  (:size dq))

(defn push-front
  "Adds an element to the front of the deque."
  [dq elt]
  (let [{:keys [front back size]} dq]
    (Deque. (cons elt front) back (inc size))))

(defn push-back
  "Adds an element to the back fo the deque."
  [dq elt]
  (let [{:keys [front back size]} dq]
    (Deque. (cons elt back) front (inc size)))
 )

(defn flip-front
  "Flip the back list to the front list, if necessary."
  [dq]
  (let [{:keys [front back size]} dq]
  (Deque. (concat (reverse back) front) ' () size)))

(defn flip-back
  "Flip the front list to the back list, if necessary."
  [dq]
  (let [{:keys [front back size]} dq]
  (Deque. ' () (concat (reverse front) back) size)))


(defn front
  "Return the front element of the deque.  May cause a flip."
  [dq]
  (let [{:keys [front back size]} dq]
  (cond (= size 0) nil)
         (empty? (seq (rest dq))) flip-front
         (empty? (seq (first dq)))))  flip-back


(def x (Deque. '(1 2 3 4) '(5 6) 7))
(front [x])




(defn back
  "Return the back element of the deque.  May cause a flip."
  [dq]
  (let [{:keys [front back size]} dq]
  (cond (= size 0) nil)
         (empty? (seq (first dq))) flip-back
         (empty? (seq (rest dq))) flip-front
))

(defn pop-front
  "Pops/dequeues an element from the front of the deque."
  [dq]
   (let [{:keys [front back size]} dq])


)

(defn pop-back
  "Pops/dequeues an element from the back of the deque."
  [dq]
  (let [{:keys [front back size]} dq])
)
