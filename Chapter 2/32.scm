(define (subsets s)
  (if (null? s)
    (list s)
    (let ((rest (subsets (cdr s))))
      (append
        rest
        (map (lambda (subs) (cons (car s) subs)) rest)))))

(display (subsets (list 1 2 3)))
(newline)
