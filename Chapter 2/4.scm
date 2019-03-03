; For not confusing with original cons function, I use pair instead.

(define (pair x y)
  (lambda (f) (f x y)))
(define (first p)
  (p (lambda (x y) x)))
(define (second p)
  (p (lambda (x y) y)))

(define p1 (pair 3 4))
(display (first p1))
(newline)
(display (second p1))
(newline)
