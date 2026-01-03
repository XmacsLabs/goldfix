;;; lexer-number-test.scm - Goldfix 数字词法分析器单元测试

(import (liii check)
        (goldfix lexer))

;; 设置测试模式
(check-set-mode! 'report-failed)

;; ============================================
;; 测试辅助函数
;; ============================================

;; 测试单个数字
(define (test-single-number input expected-lexeme expected-value)
  (let ((lexer (make-lexer input)))
    (let ((token (lexer-next-token lexer)))
      (check (number-token? token) => #t)
      (check (token-lexeme token) => expected-lexeme)
      (check (token-value token) => expected-value)
      (check (token-terminated? token) => #t)
      (check (token-has-error? token) => #f)
      ;; 检查 EOF
      (let ((eof-token (lexer-next-token lexer)))
        (check (eof-token? eof-token) => #t)))))

;; 测试多个数字
(define (test-multiple-numbers input expected-tokens)
  (let ((lexer (make-lexer input))
        (tokens '()))
    (let loop ()
      (let ((token (lexer-next-token lexer)))
        (set! tokens (cons token tokens))
        (unless (eof-token? token)
          (loop))))
    (let ((actual-tokens (reverse (cdr tokens)))) ; 去掉最后的 EOF
      (check (length actual-tokens) => (length expected-tokens))
      (for-each
       (lambda (actual expected)
         (check (number-token? actual) => #t)
         (check (token-lexeme actual) => (car expected))
         (check (token-value actual) => (cadr expected)))
       actual-tokens
       expected-tokens))))

;; ============================================
;; 测试用例开始
;; ============================================

;; 测试 1: 空输入
(let ((lexer (make-lexer "")))
  (let ((token (lexer-next-token lexer)))
    (check (eof-token? token) => #t)
    (check (token-lexeme token) => "")
    (check (token-value token) => #f)))

;; 测试 2: 整数
(test-single-number "123" "123" 123)

;; 测试 3: 小数
(test-single-number "3.14" "3.14" 3.14)

;; 测试 4: 多个数字
(test-multiple-numbers "123 456 789"
                       '(("123" 123)
                         ("456" 456)
                         ("789" 789)))

;; 测试 5: 带前导空格的数字
(let ((lexer (make-lexer "   42")))
  (let ((token (lexer-next-token lexer)))
    (check (number-token? token) => #t)
    (check (token-lexeme token) => "42")
    (check (token-value token) => 42)
    (check (token-leading-ws token) => "   ")))

;; 测试 6: 换行后的数字（更新为包含 NEWLINE token）
(let ((lexer (make-lexer "123\n456")))
  (let ((token1 (lexer-next-token lexer)))
    (check (number-token? token1) => #t)
    (check (token-lexeme token1) => "123")
    (check (token-value token1) => 123)
    (check (token-leading-ws token1) => ""))
  (let ((newline-token (lexer-next-token lexer)))
    (check (newline-token? newline-token) => #t)
    (check (token-lexeme newline-token) => "\n"))
  (let ((token2 (lexer-next-token lexer)))
    (check (number-token? token2) => #t)
    (check (token-lexeme token2) => "456")
    (check (token-value token2) => 456)
    (check (token-leading-ws token2) => "")))

;; 测试 7: 位置信息
(let ((lexer (make-lexer "123 456")))
  (let ((token1 (lexer-next-token lexer)))
    (check (token-line token1) => 1)
    (check (token-column token1) => 1)
    (check (token-offset token1) => 0))
  (let ((token2 (lexer-next-token lexer)))
    (check (token-line token2) => 1)
    (check (token-column token2) => 5)  ; "123 " 之后
    (check (token-offset token2) => 4)))

;; 测试 8: 缩进信息（更新为包含 NEWLINE token）
(let ((lexer (make-lexer "  123\n  456")))
  (let ((token1 (lexer-next-token lexer)))
    (check (number-token? token1) => #t)
    (check (token-lexeme token1) => "123")
    (check (token-indent token1) => 0)  ; 第一行没有前导空格时 indent 为 0
    (check (token-leading-ws token1) => "  "))
  (let ((newline-token (lexer-next-token lexer)))
    (check (newline-token? newline-token) => #t)
    (check (token-lexeme newline-token) => "\n"))
  (let ((token2 (lexer-next-token lexer)))
    (check (number-token? token2) => #t)
    (check (token-lexeme token2) => "456")
    (check (token-indent token2) => 2)  ; 第二行有前导空格，indent 为 2
    (check (token-leading-ws token2) => "  ")))

;; 测试 9: 错误处理（非数字字符）
(let ((lexer (make-lexer "abc")))
  (let ((token (lexer-next-token lexer)))
    (check (identifier-token? token) => #t)
    (check (token-lexeme token) => "abc")
    (check (token-has-error? token) => #f)))

;; 测试 10: 混合内容（数字和标识符）
(let ((lexer (make-lexer "123 x 456")))
  (let ((token1 (lexer-next-token lexer)))
    (check (number-token? token1) => #t)
    (check (token-lexeme token1) => "123"))
  (let ((token2 (lexer-next-token lexer)))
    (check (identifier-token? token2) => #t)
    (check (token-lexeme token2) => "x"))
  (let ((token3 (lexer-next-token lexer)))
    (check (number-token? token3) => #t)
    (check (token-lexeme token3) => "456")))

;; ============================================
;; 边界测试
;; ============================================

;; 边界测试 1: 最大整数
(test-single-number "2147483647" "2147483647" 2147483647)

;; 边界测试 2: 多个小数点（应该只允许一个）
(let ((lexer (make-lexer "123.456.789")))
  (let ((token (lexer-next-token lexer)))
    (check (number-token? token) => #t)
    (check (token-lexeme token) => "123.456")  ; 只读到第一个小数点
    (check (token-value token) => 123.456)))

;; 边界测试 3: 以小数点开头
(let ((lexer (make-lexer ".123")))
  (let ((token (lexer-next-token lexer)))
    (check (token-type token) => 'ERROR)  ; 小数点不是数字开头
    (check (token-lexeme token) => ".")))

;; 边界测试 4: 只有小数点
(let ((lexer (make-lexer ".")))
  (let ((token (lexer-next-token lexer)))
    (check (token-type token) => 'ERROR)
    (check (token-lexeme token) => ".")))

;; ============================================
;; 多进制数字测试
;; ============================================

;; 二进制数字测试
(test-single-number "#b1010" "#b1010" 10)
(test-single-number "#B1101" "#B1101" 13)

;; 八进制数字测试
(test-single-number "#o777" "#o777" 511)
(test-single-number "#O123" "#O123" 83)

;; 十进制数字（显式前缀）测试
(test-single-number "#d456" "#d456" 456)
(test-single-number "#D789" "#D789" 789)

;; 十六进制数字测试
(test-single-number "#xff" "#xff" 255)
(test-single-number "#X1A2B" "#X1A2B" 6699)

;; 无效进制前缀测试
(let ((lexer (make-lexer "#z123")))
  (let ((token (lexer-next-token lexer)))
    (check (token-type token) => 'ERROR)
    (check (token-lexeme token) => "#z")))

;; 只有 # 前缀测试
(let ((lexer (make-lexer "#")))
  (let ((token (lexer-next-token lexer)))
    (check (token-type token) => 'ERROR)
    (check (token-lexeme token) => "#")))

;; 二进制无效数字测试
;; 注意：2 不是有效的二进制数字，所以 lexer 只读取 #b10，2 作为下一个 token
(let ((lexer (make-lexer "#b102")))
  (let ((token1 (lexer-next-token lexer)))
    (check (token-type token1) => 'NUMBER)
    (check (token-lexeme token1) => "#b10")  ; 只读到有效的部分
    (check (token-value token1) => 2))
  (let ((token2 (lexer-next-token lexer)))
    (check (token-type token2) => 'NUMBER)   ; 2 是有效的十进制数字
    (check (token-lexeme token2) => "2")
    (check (token-value token2) => 2)))

;; 八进制无效数字测试
;; 注意：8 和 9 不是有效的八进制数字，所以 lexer 只读取 #o（没有数字）
;; 89 作为下一个 token（十进制数字）
(let ((lexer (make-lexer "#o89")))
  (let ((token1 (lexer-next-token lexer)))
    (check (token-type token1) => 'ERROR)    ; 没有有效数字
    (check (token-lexeme token1) => "#o"))
  (let ((token2 (lexer-next-token lexer)))
    (check (token-type token2) => 'NUMBER)   ; 89 是有效的十进制数字
    (check (token-lexeme token2) => "89")
    (check (token-value token2) => 89)))

;; 十六进制无效数字测试
;; 注意：g 不是有效的十六进制数字，所以 lexer 只读取 #x1
;; g 作为错误 token，2 作为下一个 token（十进制数字）
(let ((lexer (make-lexer "#x1g2")))
  (let ((token1 (lexer-next-token lexer)))
    (check (token-type token1) => 'NUMBER)
    (check (token-lexeme token1) => "#x1")  ; 只读到有效的部分
    (check (token-value token1) => 1))
  (let ((token2 (lexer-next-token lexer)))
    (check (identifier-token? token2) => #t)   ; g2 是标识符
    (check (token-lexeme token2) => "g2"))
  (let ((token3 (lexer-next-token lexer)))
    (check (eof-token? token3) => #t)))

;; 混合进制数字测试
(let ((lexer (make-lexer "#b101 #o777 #xff 123")))
  (let ((token1 (lexer-next-token lexer)))
    (check (number-token? token1) => #t)
    (check (token-lexeme token1) => "#b101")
    (check (token-value token1) => 5))
  (let ((token2 (lexer-next-token lexer)))
    (check (number-token? token2) => #t)
    (check (token-lexeme token2) => "#o777")
    (check (token-value token2) => 511))
  (let ((token3 (lexer-next-token lexer)))
    (check (number-token? token3) => #t)
    (check (token-lexeme token3) => "#xff")
    (check (token-value token3) => 255))
  (let ((token4 (lexer-next-token lexer)))
    (check (number-token? token4) => #t)
    (check (token-lexeme token4) => "123")
    (check (token-value token4) => 123)))

;; ============================================
;; 生成测试报告
;; ============================================

(check-report "Goldfix lexer-number 数字场景测试完成")