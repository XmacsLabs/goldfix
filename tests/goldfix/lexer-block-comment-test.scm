;;; lexer-block-comment-test.scm - Goldfix 块注释词法分析器单元测试

(import (liii check)
        (liii list)
        (goldfix lexer))

;; 设置测试模式
(check-set-mode! 'report-failed)

;; ============================================
;; 测试辅助函数
;; ============================================

;; 测试单个块注释
(define (test-single-block-comment input expected-lexeme)
  (let ((lexer (make-lexer input)))
    (let ((token (lexer-next-token lexer)))
      (check (block-comment-token? token) => #t)
      (check (token-lexeme token) => expected-lexeme)
      (check (token-value token) => #f)
      (check (token-terminated? token) => #t)
      (check (token-has-error? token) => #f)
      ;; 检查后续 token 应该是 EOF
      (let ((next-token (lexer-next-token lexer)))
        (check (eof-token? next-token) => #t)))))

;; 测试未闭合的块注释
(define (test-unterminated-block-comment input expected-lexeme)
  (let ((lexer (make-lexer input)))
    (let ((token (lexer-next-token lexer)))
      (check (unterminated-block-comment-token? token) => #t)
      (check (token-lexeme token) => expected-lexeme)
      (check (token-value token) => #f)
      (check (token-terminated? token) => #f)
      (check (token-has-error? token) => #t)
      ;; 检查后续 token 应该是 EOF
      (let ((next-token (lexer-next-token lexer)))
        (check (eof-token? next-token) => #t)))))

;; ============================================
;; 测试用例开始
;; ============================================

;; 测试 1: 简单块注释 #||#
(test-single-block-comment "#||#" "#||#")

;; 测试 2: 包含内容的块注释 #|comment|#
(test-single-block-comment "#|comment|#" "#|comment|#")

;; 测试 3: 包含空格的块注释 #| comment |#
(test-single-block-comment "#| comment |#" "#| comment |#")

;; 测试 4: 包含特殊字符的块注释 #|!@#$%^&*()|#
(test-single-block-comment "#|!@#$%^&*()|#" "#|!@#$%^&*()|#")

;; 测试 5: 包含括号的块注释 #|(define x 1)|#
(test-single-block-comment "#|(define x 1)|#" "#|(define x 1)|#")

;; 测试 6: 包含数字的块注释 #|123 abc|#
(test-single-block-comment "#|123 abc|#" "#|123 abc|#")

;; 测试 7: 包含标识符的块注释 #|define|#
(test-single-block-comment "#|define|#" "#|define|#")

;; 测试 8: 包含布尔值的块注释 #|#t #f|#
(test-single-block-comment "#|#t #f|#" "#|#t #f|#")

;; 测试 9: 包含字符的块注释 #|#\a|#
(test-single-block-comment "#|#\\a|#" "#|#\\a|#")

;; 测试 10: 包含字符串的块注释 #|"hello"|#
(test-single-block-comment "#|\"hello\"|#" "#|\"hello\"|#")

;; 测试 11: 包含行注释的块注释 #|; comment|#
(test-single-block-comment "#|; comment|#" "#|; comment|#")

;; 测试 12: 多行块注释
(test-single-block-comment "#|line 1
line 2
line 3|#" "#|line 1
line 2
line 3|#")

;; 测试 13: 块注释与空格混合 " #|comment|#"
(let ((lexer (make-lexer " #|comment|#")))
  ;; 前导空格
  (let ((ws (lexer-next-token lexer)))
    (check (whitespace-token? ws) => #t)
    (check (token-lexeme ws) => " "))
  ;; 块注释
  (let ((comment (lexer-next-token lexer)))
    (check (block-comment-token? comment) => #t)
    (check (token-lexeme comment) => "#|comment|#"))
  ;; EOF
  (let ((eof (lexer-next-token lexer)))
    (check (eof-token? eof) => #t)))

;; 测试 14: 块注释与换行符混合 "#|comment|#\n"
(let ((lexer (make-lexer "#|comment|#\n")))
  ;; 块注释
  (let ((comment (lexer-next-token lexer)))
    (check (block-comment-token? comment) => #t)
    (check (token-lexeme comment) => "#|comment|#"))
  ;; 换行符
  (let ((nl (lexer-next-token lexer)))
    (check (newline-token? nl) => #t)
    (check (token-lexeme nl) => "\n"))
  ;; EOF
  (let ((eof (lexer-next-token lexer)))
    (check (eof-token? eof) => #t)))

;; 测试 15: 嵌套块注释 #|outer #|inner|# outer|#
(test-single-block-comment "#|outer #|inner|# outer|#" "#|outer #|inner|# outer|#")

;; 测试 16: 多层嵌套块注释 #|level1 #|level2 #|level3|# level2|# level1|#
(test-single-block-comment "#|level1 #|level2 #|level3|# level2|# level1|#" "#|level1 #|level2 #|level3|# level2|# level1|#")

;; 测试 17: 嵌套块注释包含其他内容 #|a #|b|# c #|d|# e|#
(test-single-block-comment "#|a #|b|# c #|d|# e|#" "#|a #|b|# c #|d|# e|#")

;; 测试 18: 未闭合的块注释 #|comment
(test-unterminated-block-comment "#|comment" "#|comment")

;; 测试 19: 未闭合的嵌套块注释 #|outer #|inner|#
(test-unterminated-block-comment "#|outer #|inner|#" "#|outer #|inner|#")

;; 测试 20: 未闭合的多层嵌套块注释 #|a #|b #|c|#|#
(test-unterminated-block-comment "#|a #|b #|c|#|#" "#|a #|b #|c|#|#")

;; 测试 21: 块注释在文件中间 "#|comment|# define"
(let ((lexer (make-lexer "#|comment|# define")))
  ;; 块注释
  (let ((comment (lexer-next-token lexer)))
    (check (block-comment-token? comment) => #t)
    (check (token-lexeme comment) => "#|comment|#"))
  ;; 空格
  (let ((ws (lexer-next-token lexer)))
    (check (whitespace-token? ws) => #t)
    (check (token-lexeme ws) => " "))
  ;; 标识符
  (let ((ident (lexer-next-token lexer)))
    (check (identifier-token? ident) => #t)
    (check (token-lexeme ident) => "define"))
  ;; EOF
  (let ((eof (lexer-next-token lexer)))
    (check (eof-token? eof) => #t)))

;; 测试 22: 块注释包含 Unicode 字符（如果支持）
(test-single-block-comment "#|hello 世界|#" "#|hello 世界|#")

;; 测试 23: 非常长的块注释
(test-single-block-comment "#|this is a very long block comment with many characters to test the lexer's ability to handle longer comments without issues|#"
                           "#|this is a very long block comment with many characters to test the lexer's ability to handle longer comments without issues|#")

;; 测试 24: 块注释包含转义字符 #|\\n\\t|#
(test-single-block-comment "#|\\\\n\\\\t|#" "#|\\\\n\\\\t|#")

;; 测试 25: 空块注释 #||#
(test-single-block-comment "#||#" "#||#")

;; 测试 26: 块注释与行注释混合 "#|block|# ;inline"
(let ((lexer (make-lexer "#|block|# ;inline")))
  ;; 块注释
  (let ((block (lexer-next-token lexer)))
    (check (block-comment-token? block) => #t)
    (check (token-lexeme block) => "#|block|#"))
  ;; 空格
  (let ((ws (lexer-next-token lexer)))
    (check (whitespace-token? ws) => #t)
    (check (token-lexeme ws) => " "))
  ;; 行注释
  (let ((inline (lexer-next-token lexer)))
    (check (inline-comment-token? inline) => #t)
    (check (token-lexeme inline) => ";inline"))
  ;; EOF
  (let ((eof (lexer-next-token lexer)))
    (check (eof-token? eof) => #t)))

;; 测试 27: 块注释包含管道字符 #||| |#
;; 注意：#||| |#|# 不是有效的块注释，因为 |# 会结束注释
;; 所以测试 #||| |# 作为有效的块注释
(test-single-block-comment "#||| |#" "#||| |#")

;; 测试 28: 块注释包含井号字符 #| # |#
;; 注意：#|#|# 会被解析为嵌套块注释（#| 开始嵌套，|# 结束嵌套）
;; 所以测试 #| # |# 作为有效的块注释
(test-single-block-comment "#| # |#" "#| # |#")

;; 测试 29: 复杂的嵌套结构 #|a(b)c #|d(e)f|# g(h)i|#
(test-single-block-comment "#|a(b)c #|d(e)f|# g(h)i|#" "#|a(b)c #|d(e)f|# g(h)i|#")

;; 测试 30: 块注释后跟其他 token "#|comment|# (define x 1)"
(let ((lexer (make-lexer "#|comment|# (define x 1)")))
  ;; 块注释
  (let ((comment (lexer-next-token lexer)))
    (check (block-comment-token? comment) => #t)
    (check (token-lexeme comment) => "#|comment|#"))
  ;; 空格
  (let ((ws (lexer-next-token lexer)))
    (check (whitespace-token? ws) => #t)
    (check (token-lexeme ws) => " "))
  ;; 左括号
  (let ((lparen (lexer-next-token lexer)))
    (check (left-paren-token? lparen) => #t)
    (check (token-lexeme lparen) => "("))
  ;; 标识符 define
  (let ((ident1 (lexer-next-token lexer)))
    (check (identifier-token? ident1) => #t)
    (check (token-lexeme ident1) => "define"))
  ;; 空格
  (let ((ws2 (lexer-next-token lexer)))
    (check (whitespace-token? ws2) => #t)
    (check (token-lexeme ws2) => " "))
  ;; 标识符 x
  (let ((ident2 (lexer-next-token lexer)))
    (check (identifier-token? ident2) => #t)
    (check (token-lexeme ident2) => "x"))
  ;; 空格
  (let ((ws3 (lexer-next-token lexer)))
    (check (whitespace-token? ws3) => #t)
    (check (token-lexeme ws3) => " "))
  ;; 数字 1
  (let ((num (lexer-next-token lexer)))
    (check (number-token? num) => #t)
    (check (token-lexeme num) => "1")
    (check (token-value num) => 1))
  ;; 右括号
  (let ((rparen (lexer-next-token lexer)))
    (check (right-paren-token? rparen) => #t)
    (check (token-lexeme rparen) => ")"))
  ;; EOF
  (let ((eof (lexer-next-token lexer)))
    (check (eof-token? eof) => #t)))

;; 测试总结
(check-report "Goldfix lexer-block-comment 块注释场景测试完成")