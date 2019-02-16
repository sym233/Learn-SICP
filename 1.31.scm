(define (now) (js-eval "Date.now()"))

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

(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (inc2 n) (+ n 2))

(define (self n) n)

; = (n - 1)(n + 1) / n ^ 2
(define (pi-term n) 
  (/ (dec (sqr n)) (sqr n)))

;-------------------------
(define (product func a b next)
  (if (< b a)
    1
    (*
      (func a)
      (product func (next a) b next))))

(define (product-i func a b next)
  (define (prod-iter aa result)
    (if (<= aa b)
      (prod-iter (next aa) (* result (func aa)))
      result))
  (prod-iter a 1))

(define (factorial n) (product self 1 n inc))
(define (factorial-i n) (product-i self 1 n inc))

(define (calc-pi i)
  (* 4 (product pi-term 3 i inc2)))

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

(define input '((4) (5) (6) (10) (15) (18) (20)))
(define output '(24 120 720 3628800 1307674368000 6402373705728000 2432902008176640000))

(define tic (now))
(let ((test (testa 'factorial factorial)))
  (for-each
    test 
    input
    output))
(define toc (now))
(display (format "runtime: ~a ms~%" (- toc tic)))


(define tic (now))
(let ((test (testa 'factorial-iter factorial-i)))
  (for-each
    test 
    input
    output))
(define toc (now))
(display (format "runtime: ~a ms~%" (- toc tic)))

(newline)

(for-each 
  (lambda (i) (display (format "pi(~a) = ~a~%" i (calc-pi i))))
  '(10 100 1000 10000))