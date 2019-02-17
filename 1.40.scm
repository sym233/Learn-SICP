(define (both f a b) (and (f a) (f b)))
(define (either f a b) (or (f a) (f b)))
(define (float? a) (and (number? a) (not (integer? a))))

(define (== a b)
  (cond 
    ((both integer? a b)
      (= a b))
    ((and (both number? a b) (either float? a b))
      (let ((tol 1e-9))
        (<
          (abs (- a b))
          tol)))
    ((both boolean? a b)
      (boolean=? a b))
    (else #f)))

(define (sqr n) (* n n))
(define (cb n) (* n n n))

(define (inc n) (+ n 1))

;-------------------------

(define (fix-point func init no)
  (let ((next (func init)))
    (display (format "No.~a, ~a~%" no next))
    (cond 
      ((> no 100) 0)
      ((== next init) next)
      (else (fix-point func next (inc no))))))

(define (deriv g)
  (let ((dx 1e-9))
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))))

(define (newton-transform g)
  (let ((Dg (deriv g)))
    (lambda (x) (- x (/ (g x) (Dg x))))))

(define (newton-method g init)
  (fix-point (newton-transform g) init 1))

(define (cubic a b c) 
  (lambda (x) (+ (cb x) (* a (sqr x)) (* b x) c)))

;--------------------------

(newton-method (cubic -8 17 -10) -9)
(newline)
(newton-method (cubic -8 17 -10) 1.5)
(newline)
(newton-method (cubic -8 17 -10) 9)
