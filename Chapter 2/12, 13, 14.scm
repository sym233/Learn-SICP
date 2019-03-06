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

(define (make-center-percent center percent)
  (let ((width (* center percent)))
    (make-interval (- center width) (+ center width))))
(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))
(define (percent x)
  (/ (width x) (center x)))

(define (par1 r1 r2)
  (div-interval 
    (mul-interval r1 r2)
    (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
      (add-interval
        (div-interval one r1)
        (div-interval one r2)))))

(define r1 (make-center-percent 6.8 0.1))
(define r2 (make-center-percent 4.7 0.05))

(display (par1 r1 r2))
(newline) 
(display (par2 r1 r2))
(newline)
(let ((r (div-interval r1 r1)))
  (display (format "i = ~a, c = ~a, w = ~a, p = ~a" r (center r) (width r) (percent r))))
(newline) 
(let ((r (div-interval r1 r2)))
  (display (format "i = ~a, c = ~a, w = ~a, p = ~a" r (center r) (width r) (percent r))))
(newline) 

; looks that (+ (percent r1) (percent r2)) == (percent (div r1 r2))
;
