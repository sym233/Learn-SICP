(define (sq x) (* x x))
(define (cb x) (* x x x))
(define (abs x) (if (> x 0) x (- x)))
(define (closeEnough x y)
  (define tol 0.001)
  (< (abs (- x y)) tol))

;-------------------------

(define (cbrt x)
  (define (improve y) 
    (define guess (+ (/ x (sq y) 3) (/ y 1.5)))
    (if (closeEnough (cb guess) x) guess (improve guess)))
  (improve 1)
)

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

(define testf (testa 'cbrt cbrt))
(testf '(27) 3)
(testf '(64) 4)
(testf '(125) 5)
(testf '(216) 6)
