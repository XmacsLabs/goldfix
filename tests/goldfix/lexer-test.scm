;;; lexer-test.scm - Goldfix 词法分析器单元测试（数字场景）

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

;; 测试 6: 换行后的数字
(let ((lexer (make-lexer "123\n456")))
  (let ((token1 (lexer-next-token lexer)))
    (check (number-token? token1) => #t)
    (check (token-lexeme token1) => "123")
    (check (token-value token1) => 123)
    (check (token-leading-ws token1) => ""))
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

;; 测试 8: 缩进信息
(let ((lexer (make-lexer "  123\n  456")))
  (let ((token1 (lexer-next-token lexer)))
    (check (token-indent token1) => 0)  ; 第一行没有前导空格时 indent 为 0
    (check (token-leading-ws token1) => "  "))
  (let ((token2 (lexer-next-token lexer)))
    (check (token-indent token2) => 2)  ; 第二行有前导空格，indent 为 2
    (check (token-leading-ws token2) => "  ")))

;; 测试 9: 错误处理（非数字字符）
(let ((lexer (make-lexer "abc")))
  (let ((token (lexer-next-token lexer)))
    (check (token-type token) => 'ERROR)
    (check (token-lexeme token) => "a")
    (check (token-has-error? token) => #t)))

;; 测试 10: 混合内容（数字和错误）
(let ((lexer (make-lexer "123 x 456")))
  (let ((token1 (lexer-next-token lexer)))
    (check (number-token? token1) => #t)
    (check (token-lexeme token1) => "123"))
  (let ((token2 (lexer-next-token lexer)))
    (check (token-type token2) => 'ERROR)
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
;; 生成测试报告
;; ============================================

(check-report "Goldfix lexer 数字场景测试完成")