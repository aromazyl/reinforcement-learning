(ql:quickload :array-operations)

(defmacro nested-loop (syms dimensions &body body)
  "Iterates over a multidimensional range of indices.
   
   SYMS must be a list of symbols, with the first symbol
   corresponding to the outermost loop. 
   
   DIMENSIONS will be evaluated, and must be a list of 
   dimension sizes, of the same length as SYMS.

   Example:
    (nested-loop (i j) '(10 20) (format t '~a ~a~%' i j))

  "
  (unless syms (return-from nested-loop `(progn ,@body))) ; No symbols
  
  ;; Generate gensyms for dimension sizes
  (let* ((rank (length syms))
         (syms-rev (reverse syms)) ; Reverse, since starting with innermost
         (dims-rev (loop for i from 0 below rank collecting (gensym))) ; innermost dimension first
         (result `(progn ,@body))) ; Start with innermost expression
    ;; Wrap previous result inside a loop for each dimension
    (loop for sym in syms-rev for dim in dims-rev do
         (unless (symbolp sym) (error "~S is not a symbol. First argument to nested-loop must be a list of symbols" sym))
         (setf result
               `(loop for ,sym from 0 below ,dim do
                     ,result)))
    ;; Add checking of rank and dimension types, and get dimensions into gensym list
    (let ((dims (gensym)))
      `(let ((,dims ,dimensions))
         (unless (= (length ,dims) ,rank) (error "Incorrect number of dimensions: Expected ~a but got ~a" ,rank (length ,dims)))
         (dolist (dim ,dims)
           (unless (integerp dim) (error "Dimensions must be integers: ~S" dim)))
         (destructuring-bind ,(reverse dims-rev) ,dims ; Dimensions reversed so that innermost is last
           ,result)))))

;; max width
(defparameter *width* 17)

;; max height
(defparameter *height* 32)


;; define valid driving space
(defparameter *driving-space*
  (make-array (list *height* *width*) :initial-element -10))

;; 2 directions 3 speed-inc actions
(defun make-actions
    (make-array (2 3) :initial-element
     (make-Action :direction 0 :speed-inc 0 :prob 1 :rewards 0)))

;; choice actions
(defun choice-actions
    (nested-loop  ))

;; define drving policy, 4 directions do choice (+1 -1 0), speed should not over 5
;; -5-+5 speed
;; 2 directions 1: col 0 rows
;; direction 0 rows 1 cols
;; current speed -5 ~ 5
;; 2 directions actions
;; 3 choice actions
(defparameter *driving-policy*
  (make-array (list *height* *width* 2 11) :initial-element
	      (make-actions))))

;; do random policy return (state reward)
(defun do-rand-policy (state)
  (let* ((new-state
	  (if (eq? 0 (State-speed state))
	      (add-speed state (- (random 2) 1) (+ (random 5) 1))
	      (add-speed state (- (random 2) 1) (State-direction state))))
	 (new-state-reward (reward (State-position new-state))))
    (if (eq? new-state-reward -10)
	(list make-state-zero (+ -10 -1))
	(new-state new-state-reward))))

;; do greedy-policy
(defun do-greedy-policy (state policy)
  (let* ((x (Position-x (State-position state)))
	 (y (Position-y (State-position state)))
	 (speed (State-speed state))
	 (direction (State-direction state))
	 (action-pairs (aref policy x y direction (+ speed 5))))))

;; position x represents rows index ; y represents column index
(defstruct Position x y)

;; action
(defstruct Action direction speed-inc prob rewards)

;; state struct
(defstruct State position direction speed)

;; reward
(defun reward (position)
  (let ((x (Position-x position))
	(y (Position-y position)))
    (conf ((< x 0) -10)
	  ((> y *width*) -10)
	  ((> x *height*) -10)
	  ((< y 0) -10)
	  (t (aref *driving-quvalue* x y)))))

;; move-position
(defun move-position (position speed direction)
  (let ((x (Position-x position))
	(y (Position-y position)))
    (cond ((eq? direction 1)
	   (make-Position :x ((+ speed x) :y y))
	   (t (make-Position :x x :y (+ speed y)))))))

;; add-speed
(defun add-speed (state speed direction)
  (let* ((speed2 (+ (State-speed state) speed)))
    (make-State :position (move-Position (State-position state)
					 speed2
					 direction)
		:speed speed2
		:direction direction)))

;; define driving 
(defparameter *driving-qvalue*
  (make-array (list *height* *width*)
	      :initial-element (make-State
				:position (make-Position :x 0 :y 0)
				;; direction 1 vertical 2 horizon
				:direction 1
				:speed 1
				)))

;; define temp variable for calculating W
(defparameter *driving-c_w*
  (make-array (list *height* *width* 20) :initial-element 0))

(defparameter *driving-policy*
  (make-array (list *height* *width* 1) :initial-element 0))

;; init-state
(defun make-state-zero ()
  (make-State
   :position (make-Position :x (random (+ 3 (random 6))) :y 0)
   :direction 1
   :speed 0))

;; current object location
(defparameter *current-state* make-state-zero)

;; set driving space color in rows
(defun colorize-space-rows (i j h val)
  (loop for w below (- j i)
       (let ((loc (aref *driving-space* h w)))
	 (setf loc val))))

;; set driving space color in cols
(defun colorize-space-cols (i j w val)
  (loop for h below (- j i)
       (let ((loc (aref *driving-space* h w)))
	 (setf loc val))))

(defun for-loop-do (fun be en)
  (loop for i below (+ (- en be) 1)
       (fun (+ en i))))

;; initial game
(defun initalize-game (progn
			(colorize-space-rows 3 9 0 -1)
			(colorize-space-cols
			 (- *height* 6) (- *height* 1) (- *width* 1) 20)
			(colorize-space-cols
			 (- *height* 15) (- *height* 4) 0 -1)
			(colorize-space-cols
			 (- *height* 23) (- *height* 3) 1 -1)
			(for-loop-do (lambda (i)
				       (colorize-space-cols
					3 (- *height* 1) i -1))
				     2 8)
			(for-loop-do (lambda (i)
				       (colorize-space-rows
					(- *width* 8) (- *width* 1) (- *width* i) -1))
				     1 6)
			(setf (aref *driving-space* (- *height* 7) 9) -1)))

;; check current location is valid
(defun check_valid_space (h w)
  (and (<= 0 w) (>= *width* w) (<= h 0) (>= *height* h)))

;; save episode
(defparameter *state-action-reward* '())

;; generation policy
(defun soft-policy step state
  (labels ((choice-policy ))))
