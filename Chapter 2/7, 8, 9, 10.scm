(define (either f a b) (or (f a) (f b)))

(define (add-interval x y)
  (make-interval 
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let 
    (
      (p1 (* (lower-bound x) (lower-bound y)))
      (p2 (* (lower-bound x) (upper-bound y)))
      (p3 (* (upper-bound x) (lower-bound y)))
      (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval
      (min p1 p2 p3 p4)
      (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
    (let 
      (
        (u (upper-bound y))
        (l (lower-bound y)))
      (if (either zero? u l)
        (raise (format "Error: Divided by zero, (~a, ~a)" l u))
        (make-interval 
          (/ 1.0 (upper-bound y))
          (/ 1.0 (lower-bound y)))))))

(define (make-interval x y) (cons x y))

(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))

(define (sub-interval x y)
  (make-interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))

(define (width x) (/ (- (upper-bound x) (lower-bound x)) 2))

(define (width-of-add-sub x y)
  (/
    (- 
      (+ 
        (upper-bound x)
        (upper-bound y))
      (lower-bound x)
      (lower-bound y))
    2))

(define x (make-interval 2 3))
(define y (make-interval 4 5))
(define z (make-interval 0 1))

(display (width-of-add-sub x y))
(newline)
(display (width (add-interval x y)))
(newline)
(display (width (sub-interval x y)))
(newline)
(display (width (mul-interval x y)))
(newline)
(display (width (div-interval x y)))
(newline)
(display (width (div-interval x z)))
(newline)
