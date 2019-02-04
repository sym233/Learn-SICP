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

;-------------------------
(define (next-divisor a)
  (if (= a 2) 3 (+ a 2)))

(define (divides? a b) (= 0 (mod b a)))

(define (find-divisor n a)
  (cond
    ((< n (* a a)) n)
    ((divides? a n) a)
    (else (find-divisor n (next-divisor a)))))

(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n) (= n (smallest-divisor n)))

;--------------------------

(define (testa name func)
  (define (testfunc args res)
    (display (format "~a~a = " name args))
    (define r (apply func args))
    (if (== r res)
      (display (format "~a passed.~%" r))
      (display (format "~a failed! Should be ~a~%" r res))))
  (display (format "\ntesting ~a:\n" name))
  testfunc)

(define input '((9) (19) (199) (1999) (19999) (199999) (1999999)))
(define output '(#f #t #t #t #f #t #f))

(let ((test (testa 'prime? prime?)))
  (for-each test input output))
