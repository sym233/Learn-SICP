(define (fringe ls)
  (if (null? ls)
    '()
    (let ((fst (car ls)))
      (append 
        (if (list? fst)
          (fringe fst)
          (list fst))
        (fringe (cdr ls))))))

(define x (list (list 1 2) (list 3 4)))

(display (fringe x))
(newline)

(display (fringe (list x x)))
(newline)
