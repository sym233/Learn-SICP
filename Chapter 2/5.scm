(define (sqr n) (* n n))
(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (^ a b)
  (cond
    ((zero? b) 1)
    ((zero? a) 0)
    ((= a 1) 1)
    ((even? b) (sqr (^ a (/ b 2))))
    (else (* a (^ a (dec b))))))

;--------------------------------

(define (pair a b)
  (* (^ 2 a) (^ 3 b)))

(define (first p)
  (if (zero? (mod p 2))
    (inc (first (/ p 2)))
    0))

(define (second p)
  (if (zero? (mod p 3))
    (inc (second (/ p 3)))
    0))

;-----------------------------
(define p (pair 5 3))

(display (first p))
(newline)
(display (second p))
(newline)