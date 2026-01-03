;;; lexer-whitespace-test.scm - Goldfix 空白字符词法分析器单元测试

(import (liii check)
        (goldfix lexer))

;; 设置测试模式
(check-set-mode! 'report-failed)

;; ============================================
;; 测试用例开始
;; ============================================

;; 测试 1: 单个空格
(let ((lexer (make-lexer " ")))
  (let ((token (lexer-next-token lexer)))
    (check (whitespace-token? token) => #t)
    (check (token-lexeme token) => " "))
  (let ((eof-token (lexer-next-token lexer)))
    (check (eof-token? eof-token) => #t)))

;; 测试 2: 多个空格
(let ((lexer (make-lexer "   ")))
  (let ((token (lexer-next-token lexer)))
    (check (whitespace-token? token) => #t)
    (check (token-lexeme token) => "   "))
  (let ((eof-token (lexer-next-token lexer)))
    (check (eof-token? eof-token) => #t)))

;; 测试 3: 制表符
(let ((lexer (make-lexer "\t")))
  (let ((token (lexer-next-token lexer)))
    (check (whitespace-token? token) => #t)
    (check (token-lexeme token) => "\t"))
  (let ((eof-token (lexer-next-token lexer)))
    (check (eof-token? eof-token) => #t)))

;; 测试 4: 混合空格和制表符
(let ((lexer (make-lexer " \t \t")))
  (let ((token (lexer-next-token lexer)))
    (check (whitespace-token? token) => #t)
    (check (token-lexeme token) => " \t \t"))
  (let ((eof-token (lexer-next-token lexer)))
    (check (eof-token? eof-token) => #t)))

;; 测试 5: 空格后跟数字
(let ((lexer (make-lexer " 123")))
  ;; 空格token
  (let ((ws-token (lexer-next-token lexer)))
    (check (whitespace-token? ws-token) => #t)
    (check (token-lexeme ws-token) => " "))
  ;; 数字token
  (let ((num-token (lexer-next-token lexer)))
    (check (number-token? num-token) => #t)
    (check (token-lexeme num-token) => "123")
    (check (token-value num-token) => 123))
  ;; EOF
  (let ((eof-token (lexer-next-token lexer)))
    (check (eof-token? eof-token) => #t)))

;; 测试 6: 数字、空格、数字
(let ((lexer (make-lexer "123 456")))
  ;; 第一个数字
  (let ((num1 (lexer-next-token lexer)))
    (check (number-token? num1) => #t)
    (check (token-lexeme num1) => "123"))
  ;; 空格
  (let ((ws (lexer-next-token lexer)))
    (check (whitespace-token? ws) => #t)
    (check (token-lexeme ws) => " "))
  ;; 第二个数字
  (let ((num2 (lexer-next-token lexer)))
    (check (number-token? num2) => #t)
    (check (token-lexeme num2) => "456"))
  ;; EOF
  (let ((eof-token (lexer-next-token lexer)))
    (check (eof-token? eof-token) => #t)))

;; 测试 7: 换行符（NEWLINE token）
(let ((lexer (make-lexer "\n")))
  (let ((token (lexer-next-token lexer)))
    (check (newline-token? token) => #t)
    (check (token-lexeme token) => "\n"))
  (let ((eof-token (lexer-next-token lexer)))
    (check (eof-token? eof-token) => #t)))

;; 测试 8: 空格、换行、空格
(let ((lexer (make-lexer " \n ")))
  ;; 第一个空格
  (let ((ws1 (lexer-next-token lexer)))
    (check (whitespace-token? ws1) => #t)
    (check (token-lexeme ws1) => " "))
  ;; 换行
  (let ((nl (lexer-next-token lexer)))
    (check (newline-token? nl) => #t)
    (check (token-lexeme nl) => "\n"))
  ;; 第二个空格（新行的前导空格）
  (let ((ws2 (lexer-next-token lexer)))
    (check (whitespace-token? ws2) => #t)
    (check (token-lexeme ws2) => " "))
  ;; EOF
  (let ((eof-token (lexer-next-token lexer)))
    (check (eof-token? eof-token) => #t)))

;; 测试 9: Tab和空格的区别
(let ((lexer (make-lexer "\t ")))
  (let ((token (lexer-next-token lexer)))
    (check (whitespace-token? token) => #t)
    (check (token-lexeme token) => "\t "))
  (let ((eof-token (lexer-next-token lexer)))
    (check (eof-token? eof-token) => #t)))

;; ============================================
;; 生成测试报告
;; ============================================

(check-report "Goldfix lexer-whitespace 空白字符场景测试完成")
