;;; lexer-paren-test.scm - Goldfix 括号词法分析器单元测试

(import (liii check)
        (goldfix lexer))

;; 设置测试模式
(check-set-mode! 'report-failed)

;; ============================================
;; 测试辅助函数
;; ============================================

;; 测试单个括号
(define (test-single-paren input expected-type expected-lexeme)
  (let ((lexer (make-lexer input)))
    (let ((token (lexer-next-token lexer)))
      (cond
        ((eq? expected-type 'LEFT_PAREN)
         (check (left-paren-token? token) => #t))
        ((eq? expected-type 'RIGHT_PAREN)
         (check (right-paren-token? token) => #t)))
      (check (token-lexeme token) => expected-lexeme)
      (check (token-terminated? token) => #t)
      (check (token-has-error? token) => #f)
      ;; 检查 EOF
      (let ((eof-token (lexer-next-token lexer)))
        (check (eof-token? eof-token) => #t)))))

;; 测试多个括号
(define (test-multiple-parens input expected-tokens)
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
         (let ((expected-type (car expected))
               (expected-lexeme (cadr expected)))
           (cond
             ((eq? expected-type 'LEFT_PAREN)
              (check (left-paren-token? actual) => #t))
             ((eq? expected-type 'RIGHT_PAREN)
              (check (right-paren-token? actual) => #t)))
           (check (token-lexeme actual) => expected-lexeme)))
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

;; 测试 2: 单个左括号
(test-single-paren "(" 'LEFT_PAREN "(")

;; 测试 3: 单个右括号
(test-single-paren ")" 'RIGHT_PAREN ")")

;; 测试 4: 多个括号
(test-multiple-parens "()"
                      '((LEFT_PAREN "(")
                        (RIGHT_PAREN ")")))

;; 测试 5: 嵌套括号
(test-multiple-parens "(())"
                      '((LEFT_PAREN "(")
                        (LEFT_PAREN "(")
                        (RIGHT_PAREN ")")
                        (RIGHT_PAREN ")")))

;; 测试 6: 带前导空格的括号
(let ((lexer (make-lexer "   (")))
  (let ((token (lexer-next-token lexer)))
    (check (left-paren-token? token) => #t)
    (check (token-lexeme token) => "(")
    (check (token-leading-ws token) => "   ")))

;; 测试 7: 换行后的括号
(let ((lexer (make-lexer "(\n)")))
  (let ((token1 (lexer-next-token lexer)))
    (check (left-paren-token? token1) => #t)
    (check (token-lexeme token1) => "(")
    (check (token-leading-ws token1) => ""))
  (let ((newline-token (lexer-next-token lexer)))
    (check (newline-token? newline-token) => #t)
    (check (token-lexeme newline-token) => "\n"))
  (let ((token2 (lexer-next-token lexer)))
    (check (right-paren-token? token2) => #t)
    (check (token-lexeme token2) => ")")
    (check (token-leading-ws token2) => "")))

;; 测试 8: 位置信息
(let ((lexer (make-lexer "( )")))
  (let ((token1 (lexer-next-token lexer)))
    (check (left-paren-token? token1) => #t)
    (check (token-line token1) => 1)
    (check (token-column token1) => 1)
    (check (token-offset token1) => 0))
  (let ((token2 (lexer-next-token lexer)))
    (check (right-paren-token? token2) => #t)
    (check (token-line token2) => 1)
    (check (token-column token2) => 3)  ; "( " 之后
    (check (token-offset token2) => 2)))

;; 测试 9: 缩进信息（更新为包含 NEWLINE token）
(let ((lexer (make-lexer "  (\n  )")))
  (let ((token1 (lexer-next-token lexer)))
    (check (left-paren-token? token1) => #t)
    (check (token-lexeme token1) => "(")
    (check (token-indent token1) => 0)  ; 第一行没有前导空格时 indent 为 0
    (check (token-leading-ws token1) => "  "))
  (let ((newline-token (lexer-next-token lexer)))
    (check (newline-token? newline-token) => #t)
    (check (token-lexeme newline-token) => "\n"))
  (let ((token2 (lexer-next-token lexer)))
    (check (right-paren-token? token2) => #t)
    (check (token-lexeme token2) => ")")
    (check (token-indent token2) => 2)  ; 第二行有前导空格，indent 为 2
    (check (token-leading-ws token2) => "  ")))

;; 测试 10: 括号与标识符的分离
(let ((lexer (make-lexer "foo(bar)")))
  (let ((token1 (lexer-next-token lexer)))
    (check (identifier-token? token1) => #t)
    (check (token-lexeme token1) => "foo"))
  (let ((token2 (lexer-next-token lexer)))
    (check (left-paren-token? token2) => #t)
    (check (token-lexeme token2) => "("))
  (let ((token3 (lexer-next-token lexer)))
    (check (identifier-token? token3) => #t)
    (check (token-lexeme token3) => "bar"))
  (let ((token4 (lexer-next-token lexer)))
    (check (right-paren-token? token4) => #t)
    (check (token-lexeme token4) => ")")))

;; 测试 11: 括号与数字的混合
(let ((lexer (make-lexer "(123 456)")))
  (let ((token1 (lexer-next-token lexer)))
    (check (left-paren-token? token1) => #t)
    (check (token-lexeme token1) => "("))
  (let ((token2 (lexer-next-token lexer)))
    (check (number-token? token2) => #t)
    (check (token-lexeme token2) => "123"))
  (let ((token3 (lexer-next-token lexer)))
    (check (number-token? token3) => #t)
    (check (token-lexeme token3) => "456"))
  (let ((token4 (lexer-next-token lexer)))
    (check (right-paren-token? token4) => #t)
    (check (token-lexeme token4) => ")")))

;; 测试 12: 括号与布尔值的混合
(let ((lexer (make-lexer "(#t #f)")))
  (let ((token1 (lexer-next-token lexer)))
    (check (left-paren-token? token1) => #t)
    (check (token-lexeme token1) => "("))
  (let ((token2 (lexer-next-token lexer)))
    (check (boolean-token? token2) => #t)
    (check (token-lexeme token2) => "#t"))
  (let ((token3 (lexer-next-token lexer)))
    (check (boolean-token? token3) => #t)
    (check (token-lexeme token3) => "#f"))
  (let ((token4 (lexer-next-token lexer)))
    (check (right-paren-token? token4) => #t)
    (check (token-lexeme token4) => ")")))

;; 测试 13: 括号与字符的混合
(let ((lexer (make-lexer "(#\\a #\\b)")))
  (let ((token1 (lexer-next-token lexer)))
    (check (left-paren-token? token1) => #t)
    (check (token-lexeme token1) => "("))
  (let ((token2 (lexer-next-token lexer)))
    (check (character-token? token2) => #t)
    (check (token-lexeme token2) => "#\\a"))
  (let ((token3 (lexer-next-token lexer)))
    (check (character-token? token3) => #t)
    (check (token-lexeme token3) => "#\\b"))
  (let ((token4 (lexer-next-token lexer)))
    (check (right-paren-token? token4) => #t)
    (check (token-lexeme token4) => ")")))

;; 测试 14: 复杂表达式
(let ((lexer (make-lexer "(define (add x y) (+ x y))")))
  (let ((tokens '()))
    (let loop ()
      (let ((token (lexer-next-token lexer)))
        (set! tokens (cons token tokens))
        (unless (eof-token? token)
          (loop))))
    (let ((actual-tokens (reverse (cdr tokens)))) ; 去掉最后的 EOF
      ;; 调试：打印实际 token 信息
      ;(display "实际 token 数量: ") (display (length actual-tokens)) (newline)
      ;(for-each (lambda (token i)
      ;            (display i) (display ": ")
      ;            (display (token-type token)) (display " ")
      ;            (display (token-lexeme token)) (newline))
      ;          actual-tokens
      ;          (iota (length actual-tokens)))
      (check (length actual-tokens) => 13)  ; 修正为 13
      ;; 检查关键 token
      (check (left-paren-token? (list-ref actual-tokens 0)) => #t)
      (check (token-lexeme (list-ref actual-tokens 0)) => "(")
      (check (identifier-token? (list-ref actual-tokens 1)) => #t)
      (check (token-lexeme (list-ref actual-tokens 1)) => "define")
      (check (left-paren-token? (list-ref actual-tokens 2)) => #t)
      (check (token-lexeme (list-ref actual-tokens 2)) => "(")
      (check (identifier-token? (list-ref actual-tokens 3)) => #t)
      (check (token-lexeme (list-ref actual-tokens 3)) => "add")
      (check (right-paren-token? (list-ref actual-tokens 12)) => #t)  ; 修正索引为 12
      (check (token-lexeme (list-ref actual-tokens 12)) => ")"))))

;; ============================================
;; 边界测试
;; ============================================

;; 边界测试 1: 多个连续括号
(test-multiple-parens "((()))"
                      '((LEFT_PAREN "(")
                        (LEFT_PAREN "(")
                        (LEFT_PAREN "(")
                        (RIGHT_PAREN ")")
                        (RIGHT_PAREN ")")
                        (RIGHT_PAREN ")")))

;; 边界测试 2: 括号与空格混合
(test-multiple-parens "(  )  (  )"
                      '((LEFT_PAREN "(")
                        (RIGHT_PAREN ")")
                        (LEFT_PAREN "(")
                        (RIGHT_PAREN ")")))

;; 边界测试 3: 括号与换行混合（更新为包含 NEWLINE token）
(let ((lexer (make-lexer "(\n)\n(\n)")))
  (let ((token1 (lexer-next-token lexer)))
    (check (left-paren-token? token1) => #t)
    (check (token-line token1) => 1))
  (let ((newline1 (lexer-next-token lexer)))
    (check (newline-token? newline1) => #t)
    (check (token-lexeme newline1) => "\n"))
  (let ((token2 (lexer-next-token lexer)))
    (check (right-paren-token? token2) => #t)
    (check (token-line token2) => 2))
  (let ((newline2 (lexer-next-token lexer)))
    (check (newline-token? newline2) => #t)
    (check (token-lexeme newline2) => "\n"))
  (let ((token3 (lexer-next-token lexer)))
    (check (left-paren-token? token3) => #t)
    (check (token-line token3) => 3))
  (let ((newline3 (lexer-next-token lexer)))
    (check (newline-token? newline3) => #t)
    (check (token-lexeme newline3) => "\n"))
  (let ((token4 (lexer-next-token lexer)))
    (check (right-paren-token? token4) => #t)
    (check (token-line token4) => 4)))

;; 边界测试 4: 括号作为标识符的一部分（应该分离）
(let ((lexer (make-lexer "foo(bar)baz")))
  (let ((token1 (lexer-next-token lexer)))
    (check (identifier-token? token1) => #t)
    (check (token-lexeme token1) => "foo"))
  (let ((token2 (lexer-next-token lexer)))
    (check (left-paren-token? token2) => #t)
    (check (token-lexeme token2) => "("))
  (let ((token3 (lexer-next-token lexer)))
    (check (identifier-token? token3) => #t)
    (check (token-lexeme token3) => "bar"))
  (let ((token4 (lexer-next-token lexer)))
    (check (right-paren-token? token4) => #t)
    (check (token-lexeme token4) => ")"))
  (let ((token5 (lexer-next-token lexer)))
    (check (identifier-token? token5) => #t)
    (check (token-lexeme token5) => "baz")))

;; ============================================
;; 生成测试报告
;; ============================================

(check-report "Goldfix lexer-paren 括号场景测试完成")