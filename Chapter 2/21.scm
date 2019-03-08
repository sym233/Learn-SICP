(define (sq x) (* x x))

(define (square-list-1 items)
  (if (null? items)
    '()
    (cons (sq (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map sq items))


(define ls '(1 2 3 4))
(display (square-list-1 ls))
(newline)
(display (square-list-2 ls))
(newline)
