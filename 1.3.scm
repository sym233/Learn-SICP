(define (f a b c)
  (define (sq x) (* x x))
  (define (sqs x y) (+ (sq x) (sq y)))
  (cond 
    ((and (> a c) (> b c)) (sqs a b))
    ((and (> a b) (> c b)) (sqs a c))
    (else (sqs b c))))

;--------------------------
(define (test f args res)
  (display (format "test~a = " args))
  (define r (apply f args))
  (if (= r res)
    (display (format "~a passed.~%" r))
    (display (format "~a failed! Should be ~a~%" r res))))

(test f '(0 2 2) 8)
(test f '(1 2 3) 13)
(test f '(3 2 1) 13)
(test f '(2 1 3) 13)
(test f '(3 7 5) 74)
