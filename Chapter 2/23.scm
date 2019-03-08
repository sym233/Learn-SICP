(define (my-for-each f ls)
  (if (null? ls)
    #t
    (begin
      (f (car ls))
      (my-for-each f (cdr ls)))))

(for-each 
  (lambda (x) (newline) (display x))
  (list 57 321 88))
