(define (sq x) (* x x))
(define (abs x) (if (> x 0) x (- x)))
(define (closeEnough x y)
  (define tol 1e-20)
  (< (abs (- x y)) tol))

;-------------------------

(define (sqrt2 x)
  (define (improve y)
    (define newY (/ (+ y (/ x y)) 2))
    (if (closeEnough newY y) newY (improve newY)))
  (improve 1))

;--------------------------

(define (testa name func)
  (define (testfunc args res)
    (display (format "~a~a = " name args))
    (define r (apply func args))
    (if (closeEnough r res)
      (display (format "~a passed.~%" r))
      (display (format "~a failed! Should be ~a~%" r res))))
  (display (format "testing ~a:~%" name))
  testfunc)

(define testf (testa 'sqrt2 sqrt2))
(testf '(9) 3)
(testf '(0.000009) 0.003)
(testf '(9e-20) 3e-10)
(testf '(9e16) 3e8)
