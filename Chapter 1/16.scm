(define (close-enough? a b)
  (if (and (integer? a) (integer? b))
    (= a b)
    (let ((tol 1e-10))
      (<
        (abs (- a b))
        tol))))

(define (power x n)
  (if (= n 0)
    1
    (* x (power x (- n 1)))))

;-------------------------

(define (fast-pow x n)
  ; calculate x ^ n where n <- N)
  (define (pow-iter a x n)
    ; calculate a * x ^ n
    (cond
      ((= x 0) 0)
      ((= n 0) a)
      ((even? n) (pow-iter a (* x x) (/ n 2)))
      (else (pow-iter (* a x) x (- n 1)))))
  (pow-iter 1 x n))


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

(define xs '(1 2 3 4 5 1.1 1.2 1.3 2.3 5.5))
(define ns '(0 10 20 30 20 10 5 1 2 3))

(define ress (map power xs ns))
(define args (map (lambda (x n) (list x n)) xs ns))

(let ((test (testa 'pow fast-pow)))
  (for-each test args ress))
