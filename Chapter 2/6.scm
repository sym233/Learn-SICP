(define inc (lambda (x) (+ 1 x)))

(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n) 
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define (add m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

(display ((zero inc) 0))
(newline)
(display ((one inc) 0))
(newline)
(display ((two inc) 0))
(newline)
(display (((add two one) inc) 0))
(newline)
(display (((add two two) inc) 0))
(newline)
(display (((add two (add two two)) inc) 0))
(newline)
