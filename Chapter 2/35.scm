(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate 
    (lambda (x y) (+ x y)) 
    0 
    (map 
      (lambda (node)
        (if (number? node)
          1
          (count-leaves node)))
      t)))

(define x '((1 2) (3 4)))
(display (count-leaves x))
(newline)
(display (count-leaves (cons x x)))
(newline)
