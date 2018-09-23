(require 'alexandria)
(defparameter *width* 5)
(defparameter *height* 5)

(defstruct state-actions-reward
  reward-L reward-R reward-U reward-D)

(defstruct state x y is-terminal)

(defparameter *state-actions-quality*
  (make-array (list *width* *height*) :initial-element
	      (make-state-actions-reward :reward-L `(0, 0)
					 :reward-R `(0, 1)
					 :reward-U `(0, 2)
					 :reward-D `(0, 3))))
(defparameter *visited-state* '())

(defun envi-reactions (x y)
  (cond ((< x 0) (list -1 (list (+ 1 x) y)))
	((< y 0) (list -1 (list x (+ 1 y))))
	((= x *width*) (list -1 (list (- x 1) y)))
	((= y *height*) (list -1 (list x (- y 1))))
	((and (= x 0) (= y 1)) (list 10 (list 4 y)))
	((and (= x 0) (= y 3)) (list 5 (list 2 y)))
	(t (list 0 (list x y)))))

(defun epsilon-greedy (epsilon x y)
  (let* ((quality (aref *state-actions-quality* x y))
	 (reward-L (state-actions-reward-reward-L quality))
	 (reward-R (state-actions-reward-reward-R quality))
	 (reward-U (state-actions-reward-reward-U quality))
	 (reward-D (state-actions-reward-reward-D quality))
	 (max-reward (reduce #'min `(,reward-L ,reward-R ,reward-U ,reward-D) :key #'first))
	 (random-num (/ (random 10000) 10000)))
    (if (< random-num (* 3 epsilon))
	(let* ((selected (random 3)))
	  (if (> selected (cadr max-reward))
	      (+ 1 selected)
	      selected))
	(cadr max-reward))))

(defparameter *Model* (make-hash-table))
  
(defun update-model (key value) (setf (gethash key *Model*) value))

(defun update-quality (x y action learning-rate discount reward-state)
  (let* ((reward (car reward-state))
	 (state (cadr reward-state))
	 (quality (apply #'(lambda (x y) (aref *state-actions-quality* x y)) state))
	 (reward-L (state-actions-reward-reward-L quality))
	 (reward-R (state-actions-reward-reward-R quality))
	 (reward-U (state-actions-reward-reward-U quality))
	 (reward-D (state-actions-reward-reward-D quality))
	 (max-reward (reduce #'min `(,reward-L ,reward-R ,reward-L ,reward-D)))
	 (quality-DIR (cond ((= action 0) (aref (state-actions-reward-reward-L quality)))
			    ((= action 1) (aref (state-actions-reward-reward-R quality)))
			    ((= action 2) (aref (state-actions-reward-reward-U quality)))
			    (t (aref (state-actions-reward-reward-D quality))))))
    (setf quality-DIR (+ quality-DIR (* learning-rate (- (+ R (* discount max-reward)) quality-DIR))))))

(defun move (x y direction)
  (cond ((= direction 0) (list x (- y 1)))
	((= direction 1) (list x (+ y 1)))
	((= direction 2) (list (- x 1) y))
	(t (list (+ x 1) y))))

(defun get-next-state-reward (x y action)
  (let* ((reward-state (gethash (list x y action) *Model*)))
    (if reward-state
	reward-state
	(0 (let* ((move-res (move x y (random 4)))
		  (list (max (min x *width*) 0) (max (min y *height*) 0)))))))

(defun is-terminal ())

(defun run-game (epsilon rounds x y learning-rate discount)
  (if (> rounds 0)
	  (let* ((direction (epsilon-greedy epsilon x y))
		 (new-state (move x y direction))
		 (reward-state (apply #'envi-reactions new-state)))
	    (progn
	      (update-quality x y direction learning-rate discount reward-state)
	      (apply #'update-model `((,x ,y) ,direction) reward-state)
	      (if (member `(,x ,y ,direction) *visited-state*)
		  '()
		  (push `(,x ,y ,direction) *visited-state*))
	      (loop for i below 20
		   (let* ((random-seleted-state (nth (random (length *visited-state*)) *visited-state*))
			  (reward-state2 (apply #'envi-reactions (apply #'move random-seleted-state))))
		     (update-model random-seleted-state reward-state2)))
	      (- rounds 1)
	      (run-game epsilon rounds (car new-state) (cadr new-state) learning-rate discount)))
	  '()))

(run-game)
  
  

