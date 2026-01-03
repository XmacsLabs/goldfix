;; 测试括号匹配的文件
(define test1 (lambda (x)
  (+ x 1)))

;; 这里缺少一个闭括号
(define test2 (lambda (x y)
  (* x y)

;; 正常的括号匹配
(define test3 (lambda (z)
  (- z 1)))