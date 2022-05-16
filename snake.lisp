(load "/home/merov/quicklisp/setup.lisp")
(ql:quickload "bt-semaphore")
(ql:quickload "trivial-raw-io")
(defclass point ()
    ((x :initarg :x) 
    (y :initarg :y)))

(defclass game ()
    ((snake
    :initarg :snake
    :initform (list (make-instance 'point :x 2 :y 4)
                    (make-instance 'point :x 2 :y 3)
                    (make-instance 'point :x 2 :y 2)
                ))
    (direction
    :initarg :direction
    :initform 0)
    (is_pause
    :initarg :is_pause
    :initform Nil
    )
    (food
    :initarg :food
    :initform (list (make-instance 'point :x 2 :y 7) ;(make-random-state t)
                ))
    ))

(defmethod new-food((game1 game))
    (push (make-instance 'point :x (random 20) :y (random 10)) (slot-value game1 'food))
)

(defmethod growup ((game1 game))
    (let ((x (slot-value (first (slot-value game1 'snake)) 'x)) (y (slot-value (first (slot-value game1 'snake)) 'y)))
        (cond 
            ((= (slot-value game1 'direction) 0) (push (make-instance 'point :x x :y (+ y 1)) (slot-value game1 'snake))) 
            ((= (slot-value game1 'direction) 1) (push (make-instance 'point :x (+ x 1) :y y) (slot-value game1 'snake))) 
            ((= (slot-value game1 'direction) 2) (push (make-instance 'point :x x :y (- y 1)) (slot-value game1 'snake))) 
            ((= (slot-value game1 'direction) 3) (push (make-instance 'point :x (- x 1) :y y) (slot-value game1 'snake))) 
        )
    )
)

(defun cls()
  (format t "~A[H~@*~A[J" #\escape)
  (format t "~A[H~@*~A[J" #\escape))

(defun remove-food (food game1)
    (let ((new-list))
        (loop for point in (slot-value game1 'food) do
            (if (or (/= (slot-value point 'x)(slot-value food 'x)) (/= (slot-value point 'y)(slot-value food 'y)))
                (setf new-list (append (slot-value game1 'food) 
                    (list (make-instance 'point :x 3 :y 3)))
                )
            ) 
        )
        (setf (slot-value game1 'food) new-list)
    ))

(defmethod eat ((game1 game))
    (let ((head (first (slot-value game1 'snake))))
        (loop for point in (slot-value *game* 'food) do
            (if (and
                    (= (slot-value point 'x) (slot-value head 'x))
                    (= (slot-value point 'y) (slot-value head 'y))
                )
                (progn (remove-food point game1) (return T))
            )
        )
    )
)

(defmethod move ((game1 game))
    (let ((x (- (length (slot-value game1 'snake)) 1)))
        (loop repeat (- (length (slot-value game1 'snake)) 1) do
            
            (setf (slot-value (nth x (slot-value game1 'snake)) 'x)
                (slot-value (nth (- x 1) (slot-value game1 'snake)) 'x))
            
            (setf (slot-value (nth x (slot-value game1 'snake)) 'y)
                (slot-value (nth (- x 1) (slot-value game1 'snake)) 'y))
            (decf x)
        )
    )

    (cond 
        ((eql (slot-value game1 'direction) 0) (incf (slot-value (first (slot-value game1 'snake)) 'y))) 
        ((eql (slot-value game1 'direction) 1) (incf (slot-value (first (slot-value game1 'snake)) 'x))) 
        ((eql (slot-value game1 'direction) 2) (decf (slot-value (first (slot-value game1 'snake)) 'y))) 
        ((eql (slot-value game1 'direction) 3) (decf (slot-value (first (slot-value game1 'snake)) 'x))) 
    )
)
  
(defmethod run ((game1 game))
    (loop do
        (if (eql (slot-value game1 'is_pause) nil)
            (progn
                (cls)
                (if (eat game1)
                    (progn
                        (growup game1)
                        (new-food game1)
                    )
                    (move game1)
                )
                (draw-field game1)
                (setf point-food nil)
            )
        )
        (sleep 0.5)
    ))

(defmethod draw-field ((game1 game))
    (defvar width 20)
    (defvar height 10)
    (defvar y 0)
    (defvar x 0)
    (setq y 0)

    (format t "Score: 0")
    (terpri)(format t "~v@{~A~:*~}" (+ width 2) "-")
    (loop repeat height do 
        (setq x 0)
        (incf y)
        (terpri)
        (format t "~a" "|")
        (loop repeat width do 
            (incf x)
            (let ((is_snake nil)(is_food nil))
                (loop for point in (slot-value *game* 'snake) do
                    (if (and
                            (= (slot-value point 'x) x)
                            (= (slot-value point 'y) y)
                        )
                        (progn (setf is_snake T) (return))
                    )   
                )
                (loop for point in (slot-value *game* 'food) do
                    (if (and
                            (= (slot-value point 'x) x)
                            (= (slot-value point 'y) y)
                        )
                        (progn (setf is_food T) (return))
                    )
                )
                (cond
                    (is_snake (format t "-"))
                    (is_food (format t "@"))
                    (T (format t "+"))
                )
            )
        )
        (format t "~a" "|")
    )
    (terpri)(format t "~v@{~A~:*~}" (+ width 2) "-")
    (terpri)(format t "~v@{~A~:*~}" (+ width 2) "-"))
    
(defparameter *game*
    (make-instance 'game))

(defun key-down ()
    (bt:make-thread
    (lambda ()
        (let ((x nil))
            (loop
                (setf x (trivial-raw-io:read-char))
                (cond 
                    ((and (eql x #\A) (/= (slot-value *game* 'direction) 0)) 
                        (setf (slot-value *game* 'direction) 2))
                    ((and (eql x #\B) (/= (slot-value *game* 'direction) 2))
                        (setf (slot-value *game* 'direction) 0))
                    ((and (eql x #\C) (/= (slot-value *game* 'direction) 3))
                         (setf (slot-value *game* 'direction) 1))
                    ((and (eql x #\D) (/= (slot-value *game* 'direction) 1)) 
                        (setf (slot-value *game* 'direction) 3))
                    ((eql x #\Space) 
                        (setf (slot-value *game* 'is_pause) (not (slot-value *game* 'is_pause))))
                )
            )
        )
        ))
    T)

(key-down)
(run *game*)

; (let ((f '((make-instance 'point :x 2 :y 4)
;                     (make-instance 'point :x 2 :y 3)
;                     (make-instance 'point :x 2 :y 2)
;                 )))

;     (remove-nth 0 '(2 3))             
;     (print f)
; )