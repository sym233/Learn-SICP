(define (sqr n) (* n n))
(define (cb n) (* n n n))


(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (inc2 n) (+ n 2))

(define (self n) n)
(define (aver a b) (/ (+ a b) 2))


;-------------------------

(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f num)
  (if (= num 0)
    self
    (compose f (repeated f (- num 1)))))

(define (smooth f)
  (let ((dx 1e-3))
    (lambda (x) (/
      (+
        (f (- x dx))
        (f x)
        (f (+ x dx)))
      3))))

;--------------------------
(display ((double inc) 1))
(newline)
(display (((double (double double)) inc) 5))
(newline)
(display ((compose sqr inc) 6))
(newline)
(display ((repeated sqr 2) 5))
(newline)
(display ((repeated sqr 4) 2))
(newline)
(display ((smooth sqr) 5))
(newline)

