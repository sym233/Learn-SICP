(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons 
      (accumulate op init (map car seqs))
      (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map 
    (lambda (row)
      (dot-product row v))
    m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map 
      (lambda (row)
        (matrix-*-vector cols row)) 
      m)))

(define v1 '(1 2 3))
(define v2 '(2 3 4))
(define m1 '((1 2 3) (4 5 6)))
(define m2 '((1 2) (3 4) (5 6)))

(display (dot-product v1 v2))
(newline)
(display (matrix-*-vector m1 v1))
(newline)
(display (transpose m1))
(newline)
(display (matrix-*-matrix m1 m2))
(newline)
