
(define (range n)
  ; a python-like range function
  (if (= n 0)
    '()
    (append (range (- n 1)) (list (- n 1)))))

(define (listToString ls)
  ; convet '(1 2 3) to "1 2 3"
  (define str1 (format "~a" ls))
  (define len (string-length str1))
  (substring str1 1 (- len 1)))

;-------------------------

(define (getLevel n)
  (if (= n 1)
    '(1)
    ; 1st level is (1)
    ; 2nd level is (1 1)
    (let ((lastLevel (getLevel (- n 1))))
      (map
        +
        (append '(0) lastLevel)
        (append lastLevel '(0))))))

(define (pascalTriangle n)
  (define r (cdr (range (+ 1 n))))
  (define levels (map getLevel r))
  (for-each 
    (lambda(offset level)
      (display
        (format "~a~a\n"
          (make-string offset #\Space)
          (listToString level))))
    (reverse (range n))
    levels))
    

;--------------------------

(pascalTriangle 6)
