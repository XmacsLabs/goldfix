;;; lexer-boolean-test.scm - Goldfix 布尔值词法分析器单元测试

(import (liii check)
        (goldfix lexer))

;; 设置测试模式
(check-set-mode! 'report-failed)

;; ============================================
;; 测试辅助函数
;; ============================================

;; 测试单个布尔值
(define (test-single-boolean input expected-lexeme expected-value)
  (let ((lexer (make-lexer input)))
    (let ((token (lexer-next-token lexer)))
      (check (boolean-token? token) => #t)
      (check (token-lexeme token) => expected-lexeme)
      (check (token-value token) => expected-value)
      (check (token-terminated? token) => #t)
      (check (token-has-error? token) => #f)
      ;; 检查 EOF
      (let ((eof-token (lexer-next-token lexer)))
        (check (eof-token? eof-token) => #t)))))

;; 测试多个布尔值
(define (test-multiple-booleans input expected-tokens)
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
         (check (boolean-token? actual) => #t)
         (check (token-lexeme actual) => (car expected))
         (check (token-value actual) => (cadr expected)))
       actual-tokens
       expected-tokens))))

;; ============================================
;; 测试用例开始
;; ============================================

;; 测试 1: #t
(test-single-boolean "#t" "#t" #t)

;; 测试 2: #true
(test-single-boolean "#true" "#true" #t)

;; 测试 3: #f
(test-single-boolean "#f" "#f" #f)

;; 测试 4: #false
(test-single-boolean "#false" "#false" #f)

;; 测试 5: 多个布尔值
(test-multiple-booleans "#t #f #true #false"
                        '(("#t" #t)
                          ("#f" #f)
                          ("#true" #t)
                          ("#false" #f)))

;; 测试 6: 带前导空格的布尔值
(let ((lexer (make-lexer "  #t")))
  (let ((token (lexer-next-token lexer)))
    (check (boolean-token? token) => #t)
    (check (token-lexeme token) => "#t")
    (check (token-value token) => #t)
    (check (token-leading-ws token) => "  ")))

;; 测试 7: 换行后的布尔值（更新为包含 NEWLINE token）
(let ((lexer (make-lexer "#t\n#false")))
  (let ((token1 (lexer-next-token lexer)))
    (check (boolean-token? token1) => #t)
    (check (token-lexeme token1) => "#t")
    (check (token-value token1) => #t)
    (check (token-leading-ws token1) => ""))
  (let ((newline-token (lexer-next-token lexer)))
    (check (newline-token? newline-token) => #t)
    (check (token-lexeme newline-token) => "\n"))
  (let ((token2 (lexer-next-token lexer)))
    (check (boolean-token? token2) => #t)
    (check (token-lexeme token2) => "#false")
    (check (token-value token2) => #f)
    (check (token-leading-ws token2) => "")))

;; 测试 8: 位置信息
(let ((lexer (make-lexer "#t #f")))
  (let ((token1 (lexer-next-token lexer)))
    (check (token-line token1) => 1)
    (check (token-column token1) => 1)
    (check (token-offset token1) => 0))
  (let ((token2 (lexer-next-token lexer)))
    (check (token-line token2) => 1)
    (check (token-column token2) => 4)  ; "#t " 之后
    (check (token-offset token2) => 3)))

;; 测试 9: 缩进信息（更新为包含 NEWLINE token）
(let ((lexer (make-lexer "  #t\n  #f")))
  (let ((token1 (lexer-next-token lexer)))
    (check (boolean-token? token1) => #t)
    (check (token-lexeme token1) => "#t")
    (check (token-indent token1) => 0)  ; 第一行没有前导空格时 indent 为 0
    (check (token-leading-ws token1) => "  "))
  (let ((newline-token (lexer-next-token lexer)))
    (check (newline-token? newline-token) => #t)
    (check (token-lexeme newline-token) => "\n"))
  (let ((token2 (lexer-next-token lexer)))
    (check (boolean-token? token2) => #t)
    (check (token-lexeme token2) => "#f")
    (check (token-indent token2) => 2)  ; 第二行有前导空格，indent 为 2
    (check (token-leading-ws token2) => "  ")))

;; 测试 10: 无效布尔值 (#a)
(let ((lexer (make-lexer "#a")))
  (let ((token (lexer-next-token lexer)))
    (check (token-type token) => 'ERROR)
    (check (token-lexeme token) => "#a")
    (check (token-has-error? token) => #t)))

;; 测试 11: 只有 # 字符
(let ((lexer (make-lexer "#")))
  (let ((token (lexer-next-token lexer)))
    (check (token-type token) => 'ERROR)
    (check (token-lexeme token) => "#")
    (check (token-has-error? token) => #t)))

;; 测试 12: 混合内容（布尔值和标识符）
(let ((lexer (make-lexer "#t x #f")))
  (let ((token1 (lexer-next-token lexer)))
    (check (boolean-token? token1) => #t)
    (check (token-lexeme token1) => "#t"))
  (let ((token2 (lexer-next-token lexer)))
    (check (identifier-token? token2) => #t)
    (check (token-lexeme token2) => "x"))
  (let ((token3 (lexer-next-token lexer)))
    (check (boolean-token? token3) => #t)
    (check (token-lexeme token3) => "#f")))

;; ============================================
;; 布尔值和数字混合测试
;; ============================================

;; 测试 13: 布尔值和数字混合
(let ((lexer (make-lexer "#t 123 #f")))
  (let ((token1 (lexer-next-token lexer)))
    (check (boolean-token? token1) => #t)
    (check (token-lexeme token1) => "#t")
    (check (token-value token1) => #t))
  (let ((token2 (lexer-next-token lexer)))
    (check (number-token? token2) => #t)
    (check (token-lexeme token2) => "123")
    (check (token-value token2) => 123))
  (let ((token3 (lexer-next-token lexer)))
    (check (boolean-token? token3) => #t)
    (check (token-lexeme token3) => "#f")
    (check (token-value token3) => #f)))

;; 测试 14: 布尔值和多进制数字混合
(let ((lexer (make-lexer "#t #b101 #false #xff")))
  (let ((token1 (lexer-next-token lexer)))
    (check (boolean-token? token1) => #t)
    (check (token-lexeme token1) => "#t")
    (check (token-value token1) => #t))
  (let ((token2 (lexer-next-token lexer)))
    (check (number-token? token2) => #t)
    (check (token-lexeme token2) => "#b101")
    (check (token-value token2) => 5))
  (let ((token3 (lexer-next-token lexer)))
    (check (boolean-token? token3) => #t)
    (check (token-lexeme token3) => "#false")
    (check (token-value token3) => #f))
  (let ((token4 (lexer-next-token lexer)))
    (check (number-token? token4) => #t)
    (check (token-lexeme token4) => "#xff")
    (check (token-value token4) => 255)))

;; 测试 15: 区分布尔值和数字 (#t vs #b101)
(let ((lexer (make-lexer "#t#b101")))
  (let ((token1 (lexer-next-token lexer)))
    (check (boolean-token? token1) => #t)
    (check (token-lexeme token1) => "#t")
    (check (token-value token1) => #t))
  (let ((token2 (lexer-next-token lexer)))
    (check (number-token? token2) => #t)
    (check (token-lexeme token2) => "#b101")
    (check (token-value token2) => 5)))

;; 测试 16: 区分布尔值和数字 (#false vs #xff)
(let ((lexer (make-lexer "#false#xff")))
  (let ((token1 (lexer-next-token lexer)))
    (check (boolean-token? token1) => #t)
    (check (token-lexeme token1) => "#false")
    (check (token-value token1) => #f))
  (let ((token2 (lexer-next-token lexer)))
    (check (number-token? token2) => #t)
    (check (token-lexeme token2) => "#xff")
    (check (token-value token2) => 255)))

;; ============================================
;; 边界测试
;; ============================================

;; 边界测试 1: 布尔值后跟其他字符 (#tx)
(let ((lexer (make-lexer "#tx")))
  (let ((token1 (lexer-next-token lexer)))
    (check (boolean-token? token1) => #t)
    (check (token-lexeme token1) => "#t")
    (check (token-value token1) => #t))
  (let ((token2 (lexer-next-token lexer)))
    (check (identifier-token? token2) => #t)
    (check (token-lexeme token2) => "x")))

;; 边界测试 2: 布尔值后跟数字 (#t123)
(let ((lexer (make-lexer "#t123")))
  (let ((token1 (lexer-next-token lexer)))
    (check (boolean-token? token1) => #t)
    (check (token-lexeme token1) => "#t")
    (check (token-value token1) => #t))
  (let ((token2 (lexer-next-token lexer)))
    (check (number-token? token2) => #t)
    (check (token-lexeme token2) => "123")
    (check (token-value token2) => 123)))

;; 边界测试 3: 大写 T 不是布尔值 (#T)
(let ((lexer (make-lexer "#T")))
  (let ((token (lexer-next-token lexer)))
    (check (token-type token) => 'ERROR)
    (check (token-lexeme token) => "#T")))

;; 边界测试 4: #true 的部分匹配 (#tr)
(let ((lexer (make-lexer "#tr")))
  (let ((token (lexer-next-token lexer)))
    (check (boolean-token? token) => #t)
    (check (token-lexeme token) => "#t")
    (check (token-value token) => #t))
  (let ((token2 (lexer-next-token lexer)))
    (check (identifier-token? token2) => #t)
    (check (token-lexeme token2) => "r")))

;; 边界测试 5: #false 的部分匹配 (#fal)
(let ((lexer (make-lexer "#fal")))
  (let ((token (lexer-next-token lexer)))
    (check (boolean-token? token) => #t)
    (check (token-lexeme token) => "#f")
    (check (token-value token) => #f))
  (let ((token2 (lexer-next-token lexer)))
    (check (identifier-token? token2) => #t)
    (check (token-lexeme token2) => "al"))
  (let ((token3 (lexer-next-token lexer)))
    (check (eof-token? token3) => #t)))

;; 边界测试 6: #true 后跟其他字符 (#truex)
(let ((lexer (make-lexer "#truex")))
  (let ((token1 (lexer-next-token lexer)))
    (check (boolean-token? token1) => #t)
    (check (token-lexeme token1) => "#true")
    (check (token-value token1) => #t))
  (let ((token2 (lexer-next-token lexer)))
    (check (identifier-token? token2) => #t)
    (check (token-lexeme token2) => "x")))

;; 边界测试 7: #false 后跟其他字符 (#falsey)
(let ((lexer (make-lexer "#falsey")))
  (let ((token1 (lexer-next-token lexer)))
    (check (boolean-token? token1) => #t)
    (check (token-lexeme token1) => "#false")
    (check (token-value token1) => #f))
  (let ((token2 (lexer-next-token lexer)))
    (check (identifier-token? token2) => #t)
    (check (token-lexeme token2) => "y")))

;; ============================================
;; 生成测试报告
;; ============================================

(check-report "Goldfix lexer-boolean 布尔值场景测试完成")