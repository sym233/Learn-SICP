(define (close-enough? a b)
  (if (and (integer? a) (integer? b))
    (= a b)
    (let ((tol 1e-10))
      (<
        (abs (- a b))
        tol))))

(define (double x) (* 2 x))
(define (halve x) (/ x 2))
;-------------------------

(define (mul a b)
  (cond
    ((= b 0) 0)
    ((= b 1) a)
    ((< a b) (mul b a))
    ((even? b) (mul (double a) (halve b)))
    (else (+ (mul a (- b 1)) a))))


;--------------------------

(define (testa name func)
  (define (testfunc args res)
    (display (format "~a~a = " name args))
    (define r (apply func args))
    (if (close-enough? r res)
      (display (format "~a passed.~%" r))
      (display (format "~a failed! Should be ~a~%" r res))))
  (display (format "testing ~a:~%" name))
  testfunc)

(define as '(1 2 3 4 5 6 7 8 9 0))
(define bs '(0 10 20 30 20 10 5 1 2 3))

(define ress (map * as bs))
(define args (map (lambda (a b) (list a b)) as bs))

(let ((test (testa 'mul mul)))
  (for-each test args ress))
