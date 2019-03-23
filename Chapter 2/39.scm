(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))
  
;-----------------------------------

(define (reverse-1 sequence)
  (fold-right 
    (lambda (x y) 
      (append y (list x)))
    '()
    sequence))

(define (reverse-2 sequence)
  (fold-left 
    (lambda (x y)
      (cons y x))
    '()
    sequence))

;------------------------------------

(define s1 '())
(define s2 '(1))
(define s3 '(1 2 3))
(display (reverse-1 s1))
(newline)
(display (reverse-1 s2))
(newline)
(display (reverse-1 s3))
(newline)
(display (reverse-2 s1))
(newline)
(display (reverse-2 s2))
(newline)
(display (reverse-2 s3))
(newline)