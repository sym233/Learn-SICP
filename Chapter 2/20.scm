(define (same-parity first . ls)
  (cons
    first
    (filter (lambda (num) (even? (- first num))) ls)))

(display (same-parity 1 2 3 4 5 6 7))
(newline)
(display (same-parity 2 3 4 5 6 7))
(newline)
