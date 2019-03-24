(define (both f a b) (and (f a) (f b)))
(define (every f seq)
  (cond 
    ((null? seq) #t)
    ((not (f (car seq))) #f)
    (else (every f (cdr seq)))))

(define (all seq) (every self seq))
(define (!= a b) (not (= a b)))

(define (map-i func-item-index seq)
  (let ((r (range 0 (length seq))))
    (map func-item-index seq r)))

(define (filter-map f g seq)
; equivalent to (map g (filter f seq))
  (cond
    ((null? seq) '())
    ((f (car seq))
      (cons
        (g (car seq))
        (filter-map f g (cdr seq))))
    (else (filter-map f g (cdr seq)))))

(define (self x) x)
(define (inc n) (+ 1 n))
(define (dec n) (- n 1))
;-----------------------------------


(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (range begin end)
  (if (= begin end)
    '()
    (cons begin (range (inc begin) end))))

;-----
; board <- (list)
; board[i] => a queen in [i, board[i]]

(define (new-queen-available? board new-queen-position)
  (and 
    (every
      ; safe in horizontal directions
      (lambda (position)
        (!= position new-queen-position))
      board)
    (let ((new-queen-row (length board)))
      (all (map-i 
        ; safe in diagonal directions
        (lambda (position row) 
          (!= 
            (abs (- new-queen-position position))
            (- new-queen-row row)))
        board)))))

(define (queens board-size)
  (define empty-board '())
  (define (nth-queen n)
    (if (zero? n)
      (list empty-board)
      (let 
        (
          (previous-boards (nth-queen(dec n)))
          (new-positions (range 0 board-size)))
        (flatmap 
          (lambda (board)
            (filter-map 
              (lambda (position)
                (new-queen-available? board position))
              (lambda (position)
                (append board (list position)))
              new-positions))
          previous-boards))))
  (nth-queen board-size))
  
(define (print-board board)
  (define size (length board))
  (define r (range 0 size))
  (display 
    (apply string-append 
      (map 
        (lambda (row) 
          (list->string (append 
            (map 
              (lambda (pos)
                (if (= pos row)
                  #\Q
                  #\_))
              r)
            (list #\newline))))
      board)))
  (newline))

(define q8 (queens 8))
(for-each print-board q8)
(display (format "total cases: ~a~%" (length q8)))
