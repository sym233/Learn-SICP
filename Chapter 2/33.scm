(define (sq x) (* x x))
(define (inc n) (+ 1 n))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))

(display (map sq '(1 2 3)))
(newline)
(display (append '(1 2 3) '(4 5 6)))
(newline)
(display (length '(1 2 3 4 5 6)))
(newline)
