(define (every? f ls)
  (cond
    ((not (list? ls)) (f ls))
    ((= (length ls) 0) #t)
    ((f (car ls)) (every? f (cdr ls)))
    (else #f)))

(define (range n)
  ; a python-like range function
  (if (= n 0)
    '()
    (append (range (- n 1)) (list (- n 1)))))

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

(define (sqr n) (* n n))


;-------------------------

; calc b ^ e % m
(define (fast-expmod b e m)
  (cond 
    ((= e 0) (mod 1 m))
    ((even? e)
      (mod 
        (sqr (fast-expmod b (/ e 2) m))
        m))
    (else
      (mod
        (* (mod b m) (fast-expmod b (- e 1) m))
        m))))

(define (carmichael-test? n)
  (define nums (cdr (range n)))
  (define enums (map (lambda (a) (fast-expmod a n n)) nums))
  (every? (lambda (a) a) (map = nums enums)))

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

(define input '((99) (561) (1105) (1729) (2465) (2821) (6601)))
(define output '(#f #t #t #t #t #t #t))

(let ((test (testa 'carmichael-test? carmichael-test?)))
  (for-each test input output))
