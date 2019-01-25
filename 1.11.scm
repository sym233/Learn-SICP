;-------------------------
(define (f_recursive n)
  (if (< n 3)
    n 
    (+
      (f_recursive (- n 1))
      (* 2 (f_recursive (- n 2)))
      (* 3 (f_recursive (- n 3))))))

(define (f_iterative n)
  (if (< n 3)
    n
    (begin
      (define (f_iter f_n_3 f_n_2 f_n_1 currentN)
        (if (<= currentN n)
          (f_iter
            f_n_2 
            f_n_1
            (+ 
              f_n_1
              (* 2 f_n_2)
              (* 3 f_n_3))
            (+ currentN 1))
          f_n_1))
      (f_iter 0 1 2 3))))

  

;--------------------------

(define (testa name func)
  (define (testfunc args res)
    (display (format "~a~a = " name args))
    (define r (apply func args))
    (if (= r res)
      (display (format "~a passed.~%" r))
      (display (format "~a failed! Should be ~a~%" r res))))
  (display (format "\ntesting ~a:\n" name))
  testfunc)

(define input '((0) (1) (3) (4) (6) (8) (10) (13) (15)))
(define output '(0 1 4 11 59 335 1892 25315 142717))

(let ((test (testa 'f_r f_recursive)))
  (for-each test input output))

(let ((test (testa 'f_i f_iterative)))
  (for-each test input output))
