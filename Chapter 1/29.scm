(define (both f a b) (and (f a) (f b)))
;-----------------------------------

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)
(define (mobile? m) (not (number? m)))

(define (total-weight mobile)
  (if (mobile? mobile)
    (+
      (total-weight (branch-structure (left-branch mobile)))
      (total-weight (branch-structure (right-branch mobile))))
    mobile))

(define (torque branch)
  (*
    (branch-length branch)
    (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (if (mobile? mobile)
    (let 
      (
        (left-torque (torque (left-branch mobile)))
        (right-torque (torque (right-branch mobile))))
      (if (= left-torque right-torque)
        (both
          balanced?
          (branch-structure (left-branch mobile))
          (branch-structure (right-branch mobile)))
        #f))
    ; else of (mobile? mobile)
    #t))


;----------------------------------------
;                  |
;        10--------+--3          <- b5 - m3 - b6
;        3            |   
;               6-----+-----6    <- b3 - m2 - b4
;               |           5
;         6-----+---4            <- b1 - m1 - b2
;         2         3
;
(define b1 (make-branch 6 2))
(define b2 (make-branch 4 3))
(define m1 (make-mobile b1 b2))
(define b3 (make-branch 6 m1))
(define b4 (make-branch 6 5))
(define m2 (make-mobile b3 b4))
(define b5 (make-branch 10 3))
(define b6 (make-branch 3 m2))
(define m3 (make-mobile b5 b6))

(display (total-weight m3))
(newline)
(display (balanced? m3))
(newline)

;
;                 |
;        9--------+--3          <- b9 - m5 - b10
;        3           |   
;              6\\\\\+\\\\\6    <- b7 - m4 - b8
;              4           5

(define b7 (make-branch 6 4))
(define b8 (make-branch 6 5))
(define m4 (make-mobile b7 b8))
(define b9 (make-branch 9 3))
(define b10 (make-branch 3 m4))
(define m5 (make-mobile b9 b10))
(display (total-weight m5))
(newline)
(display (balanced? m5))
(newline)
