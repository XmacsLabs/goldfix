;; 括号匹配正确的测试文件
(define (square x)
  (* x x))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (sum-list lst)
  (if (null? lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))