(define (inc n) (+ 1 n))
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

(define (multi . lists)
  (if (null? lists)
    '()
    (let ((first-list (car lists)) (remain-lists (cdr lists)))
      (if (null? remain-lists)
        (map list first-list)
        (let ((rem (apply multi remain-lists)))
          (flatmap
            (lambda (item)
              (map 
                (lambda (rem-item)
                  (cons item rem-item))
                rem))
            first-list))))))


(define (ordered? l)
  (cond 
    ((null? l) #t)
    ((null? (cdr l)) #t)
    ((< (car l) (cadr l)) (ordered? (cdr l)))
    (else #f)))
(define (sum l) (accumulate + 0 l))

(define (ordered-triples n s)
  (let ((r (range 1 (inc n))))
    (let ((triples (multi r r r)))
      (filter 
        (lambda (l) 
          (and (ordered? l) (= s (sum l))))
        triples))))
        
;------------------------------

(display (ordered-triples 7 10))
(newline)