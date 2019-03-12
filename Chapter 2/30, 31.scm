(define (sq x) (* x x))

;-------------------------------

(define leaf? number?)
(define (square-tree-1 tree)
  (cond
    ((null? tree) tree)
    ((leaf? tree) (sq tree))
    (else (cons
      (square-tree-1 (car tree)) 
      (square-tree-1 (cdr tree))))))


(define (square-tree-2 tree)
  (if (leaf? tree)
    (sq tree)
    (map square-tree-2 tree)))

(define (tree-map f tree)
  (if (leaf? tree)
    (f tree)
    (map (lambda (t) (tree-map f t)) tree)))

(define (square-tree-3 tree) (tree-map sq tree))

;-------------------------------

(define t 
  (list 1
    (list 2 (list 3 4) 5)
    (list 6 7)))

(display (square-tree-1 t))
(newline)
(display (square-tree-2 t))
(newline)
(display (square-tree-3 t))
(newline)
