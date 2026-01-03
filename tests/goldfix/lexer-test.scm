;;; lexer-test.scm - Goldfix 词法分析器主模块测试

(import (liii check)
        (goldfix lexer))

;; 设置测试模式
(check-set-mode! 'report-failed)

;; 测试主模块是否正常工作
(let ((lexer (make-lexer "123 456")))
  (let ((token1 (lexer-next-token lexer)))
    (check (number-token? token1) => #t)
    (check (token-lexeme token1) => "123")
    (check (token-value token1) => 123))
  (let ((token2 (lexer-next-token lexer)))
    (check (number-token? token2) => #t)
    (check (token-lexeme token2) => "456")
    (check (token-value token2) => 456))
  (let ((eof-token (lexer-next-token lexer)))
    (check (eof-token? eof-token) => #t)))

;; 测试多进制数字
(let ((lexer (make-lexer "#b101 #xff 789")))
  (let ((token1 (lexer-next-token lexer)))
    (check (number-token? token1) => #t)
    (check (token-lexeme token1) => "#b101")
    (check (token-value token1) => 5))
  (let ((token2 (lexer-next-token lexer)))
    (check (number-token? token2) => #t)
    (check (token-lexeme token2) => "#xff")
    (check (token-value token2) => 255))
  (let ((token3 (lexer-next-token lexer)))
    (check (number-token? token3) => #t)
    (check (token-lexeme token3) => "789")
    (check (token-value token3) => 789)))

;; 测试 NEWLINE token
(let ((lexer (make-lexer "123\n456")))
  (let ((token1 (lexer-next-token lexer)))
    (check (number-token? token1) => #t)
    (check (token-lexeme token1) => "123")
    (check (token-value token1) => 123))
  (let ((newline-token (lexer-next-token lexer)))
    (check (newline-token? newline-token) => #t)
    (check (token-lexeme newline-token) => "\n")
    (check (token-line newline-token) => 1)
    (check (token-column newline-token) => 4))  ; 123 在第4列结束
  (let ((token2 (lexer-next-token lexer)))
    (check (number-token? token2) => #t)
    (check (token-lexeme token2) => "456")
    (check (token-value token2) => 456)
    (check (token-line token2) => 2)  ; 第二行
    (check (token-column token2) => 1))  ; 第二行第一列
  (let ((eof-token (lexer-next-token lexer)))
    (check (eof-token? eof-token) => #t)))

;; 测试多个 NEWLINE token
(let ((lexer (make-lexer "a\n\nb")))
  (let ((token1 (lexer-next-token lexer)))
    (check (identifier-token? token1) => #t)
    (check (token-lexeme token1) => "a"))
  (let ((newline1 (lexer-next-token lexer)))
    (check (newline-token? newline1) => #t)
    (check (token-lexeme newline1) => "\n"))
  (let ((newline2 (lexer-next-token lexer)))
    (check (newline-token? newline2) => #t)
    (check (token-lexeme newline2) => "\n"))
  (let ((token2 (lexer-next-token lexer)))
    (check (identifier-token? token2) => #t)
    (check (token-lexeme token2) => "b"))
  (let ((eof-token (lexer-next-token lexer)))
    (check (eof-token? eof-token) => #t)))

(check-report "Goldfix lexer 主模块测试完成")