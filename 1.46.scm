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

(define (inc n) (+ n 1))

;-------------------------

(define (iterative-improve good-enough? improve)
  (define (iter x . no)
    (cond
      ((null? no) (iter x 1))
      ((> (car no) 100) (begin
        (display "too many iteration\n")
        0))
      ((good-enough? x) x)
      (else (begin
        (define nextx (improve x))
        (display (format "~a, x = ~a~%" (car no) nextx))
        (iter nextx (inc (car no)))))))
  iter)

(define (fix-point f init)
  (define good-enough? (lambda (x) (== (f x) x)))
  ((iterative-improve good-enough? f) init))

(define (sqrt y) 
  (define f (lambda (x) (/ (+ x (/ y  x)) 2)))
  (fix-point f 1))
;--------------------------

(display (fix-point cos 1))
(newline)

(display (sqrt 100))
(newline)
