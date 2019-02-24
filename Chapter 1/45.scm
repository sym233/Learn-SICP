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
(define (self n) n)
(define (aver a b) (/ (+ a b) 2))

(define (^ a b)
  (cond
    ((= b 0) 1)
    ((= a 0) 0)
    ((= a 1) 1)
    ((even? b) (sqr (^ a (/ b 2))))
    (else (* a (^ a (- b 1))))))

;-------------------------

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f num)
  (if (= num 0)
    self
    (compose f (repeated f (- num 1)))))

(define (damp f)
  (lambda (val) (aver (f val) val)))

(define (fix-point func init no)
  (let ((next (func init)))
    (display (format "No.~a, ~a~%" no next))
    (cond ((>= no 100) (begin
      (display "too many iterations\n")
      0))
    ((== next init) next)
    (else (fix-point func next (inc no))))))

(define (root n)
  (lambda (x)
    (lambda (y)
      (/ x (^ y (- n 1))))))

;--------------------------
(display (format "*** 100 ^ 1/2 = ~a~%"
  (fix-point 
    ((repeated damp 1) ((root 2) 100)) 1 1)))

(display (format "*** 100 ^ 1/3 = ~a~%"
  (fix-point 
    ((repeated damp 1) ((root 3) 100)) 1 1)))

(display (format "*** 100 ^ 1/4 = ~a~%"
  (fix-point 
    ((repeated damp 2) ((root 4) 100)) 1 1)))

(display (format "*** 100 ^ 1/8 = ~a~%"
  (fix-point 
    ((repeated damp 3) ((root 8) 100)) 1 1)))


