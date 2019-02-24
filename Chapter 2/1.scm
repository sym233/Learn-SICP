(define (sign n)
  (cond
    ((positive? n) 1)
    ((negative? n) -1)
    (else 0)))

(define (gcd a b)
  (cond
    ((= a b) a)
    ((< a b) (gcd b a))
    ((zero? b) 0)
    ((zero? (mod a b)) b)
    (else (gcd (mod a b) b))))
;-------------------------

(define (make-rat num den)
  (define s (* (sign num) (sign den)))
  (define g (gcd (abs num) (abs den)))
  (cond 
    ((zero? den) (raise Error: 0))
    ((zero? num) (cons num (abs den)))
    (else (cons (/ (abs num) g s) (/ (abs den) g)))))

(define (disp-rat r)
  (display (format "~a / ~a~%" (car r) (cdr r))))

;-------------------------

(disp-rat (make-rat 1 12))
(disp-rat (make-rat -2 12))
(disp-rat (make-rat 3 -12))
(disp-rat (make-rat -4 -12))