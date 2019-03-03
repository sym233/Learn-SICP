(define (average . nums)
  (if (null? nums)
    0
    (/ (apply + nums) (length nums))))

;-------------------------

(define (make-point x y)  (cons x y))
(define x-point car)
(define y-point cdr)

(define (print-point p) 
  (let ((x (x-point p)) (y (y-point p)))
    (display (format "(~a, ~a)~%" x y))))

(define (make-segment point-start point-end)
  (cons point-start point-end))
(define segment-start car)
(define segment-end cdr)

(define (midpoint-segment seg)
  (let ((s (segment-start seg)) (e (segment-end seg)))
    (let (
      (sx (x-point s))
      (sy (y-point s))
      (ex (x-point e)) 
      (ey (y-point e)))
      
      (make-point (average sx ex) (average sy ey)))))



;-------------------------

(define p1 (make-point 1 3))
(define p2 (make-point 2 4))
(define s1 (make-segment p1 p2))
(define p3 (midpoint-segment s1))

(print-point p3)
