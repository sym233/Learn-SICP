(define (make-point x y)  (cons x y))
(define x-point car)
(define y-point cdr)

;-------------------------

(define (make-rect left-top-point right-bottom-point)
  (cons left-top-point right-bottom-point))

(define (length-of-rect rect)
  (let ((ltp (car rect)) (rbp (cdr rect)))
    (let ((ltpx (car ltp)) (rbpx (car rbp)))
      (- rbpx ltpx))))
(define (width-of-rect rect)
  (let ((ltp (car rect)) (rbp (cdr rect)))
    (let ((ltpy (cdr ltp)) (rbpy (cdr rbp)))
      (- ltpy rbpy))))

(define (area-of-rect rect)
  (let ((l (length-of-rect rect)) (w (width-of-rect rect)))
    (* l w)))
    
(define (perimeter-of-rect rect)
  (let ((l (length-of-rect rect)) (w (width-of-rect rect)))
    (+ l l w w)))

;-------------------------

(define p1 (make-point 1 3))
(define p2 (make-point 3 1))

(define r1 (make-rect p1 p2))
(define s (area-of-rect r1))
(define c (perimeter-of-rect r1))

(display (format "perimeter = ~a, area = ~a~%" c s))