(define (both f a b) (and (f a) (f b)))


(define (next-divisor a)
  (if (= a 2) 3 (+ a 2)))

(define (divides? a b) (zero? (mod b a)))

(define (find-divisor n a)
  (cond
    ((< n (* a a)) n)
    ((divides? a n) a)
    (else (find-divisor n (next-divisor a)))))

(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n) (= n (smallest-divisor n)))


(define (inc n) (+ 1 n))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

;-----------------------------------
(define (range begin end)
  (if (= begin end)
    '()
    (cons begin (range (inc begin) end))))

(define (unique-pairs n)
  (fold-left 
    append
    '()
    (map 
      (lambda (i)
        (map (lambda (j) (cons i j)) (range 1 i)))
      (range 1 (inc n)))))

(define (prime-sum-pairs n)
  (let ((pairs (unique-pairs n)))
    (filter 
      (lambda (p)
        (let ((s (+ (car p) (cdr p))))
          (prime? s)
        ))
      pairs)))

(display (prime-sum-pairs 6))
(newline)