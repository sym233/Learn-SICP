(define (last-pair ls)
  (cond
    ((null? ls) ls)
    ((null? (cdr ls)) ls)
    (else (last-pair (cdr ls)))))

(display (last-pair (list 23 72 149 34)))
(newline)

(define (reverse ls)
  (define (reverse-iter ls rls)
    (if (null? ls)
      rls
      (reverse-iter (cdr ls) (cons (car ls) rls))))
  (reverse-iter ls '()))

(display (reverse (list 23 72 149 34)))
(newline)

(define (deep-reverse ls)
  (define (reverse-iter ls rls)
    (if (null? ls)
      rls
      (let ((fst (car ls)))
        (reverse-iter (cdr ls) (cons (if (list? fst) (deep-reverse fst) fst) rls)))))
  (reverse-iter ls '()))