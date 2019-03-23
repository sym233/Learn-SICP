(define (both f a b) (and (f a) (f b)))
(define (either f a b) (or (f a) (f b)))
(define (float? a) (and (number? a) (not (integer? a))))

(define (== a b)
  (cond 
    ((both integer? a b)
      (= a b))
    ((and (both number? a b) (either float? a b))
      (let ((tol 1e-10))
        (<
          (abs (- a b))
          tol)))
    ((both boolean? a b)
      (boolean=? a b))
    (else #f)))

(define (aver a b) (/ (+ a b) 2))

;-------------------------

(define (fix-point func init no)
  (let ((next (func init)))
    (display (format "No.~a, ~a~%" no next))
    (if (== next init)
      next
      (fix-point func next (inc no)))))

(define (fix-point-d func init no)
  (let ((next (func init)))
    (display (format "No.~a, ~a~%" no next))
    (if (== next init)
      next
      (fix-point-d func (aver init next) (inc no)))))

;--------------------------

(define phi (lambda (x) (+ 1 (/ 1 x))))
(define f36 (lambda (x) (/ (log 1000) (log x))))

(display "\ntest phi without damping:\n")
(fix-point phi 1 1)
(display "\ntest phi with damping:\n")
(fix-point-d phi 1 1)
(display "\ntest f36 without damping:\n")
(fix-point f36 2 1)
(display "\ntest f36 with damping:\n")
(fix-point-d f36 2 1)
