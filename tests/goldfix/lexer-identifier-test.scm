;;; lexer-identifier-test.scm - Goldfix 标识符词法分析测试

(import (liii check)
        (goldfix lexer-identifier)
        (goldfix lexer))

;; 设置测试模式
(check-set-mode! 'report-failed)

;; ============================================
;; 测试字符分类函数
;; ============================================

;; 测试 Initial 字符
(check (initial-char? #\a) => #t)
(check (initial-char? #\Z) => #t)
(check (initial-char? #\!) => #t)
(check (initial-char? #\$) => #t)
(check (initial-char? #\%) => #t)
(check (initial-char? #\&) => #t)
(check (initial-char? #\*) => #t)
(check (initial-char? #\/) => #t)
(check (initial-char? #\:) => #t)
(check (initial-char? #\<) => #t)
(check (initial-char? #\=) => #t)
(check (initial-char? #\>) => #t)
(check (initial-char? #\?) => #t)
(check (initial-char? #\^) => #t)
(check (initial-char? #\_) => #t)
(check (initial-char? #\~) => #t)

;; 测试非 Initial 字符
(check (initial-char? #\1) => #f)
(check (initial-char? #\+) => #f)
(check (initial-char? #\-) => #f)
(check (initial-char? #\.) => #f)
(check (initial-char? #\@) => #f)
(check (initial-char? #\() => #f)
(check (initial-char? #\)) => #f)

;; 测试 Subsequent 字符
(check (subsequent-char? #\a) => #t)    ; Initial
(check (subsequent-char? #\!) => #t)    ; Initial
(check (subsequent-char? #\1) => #t)    ; 数字
(check (subsequent-char? #\+) => #t)    ; 显式符号
(check (subsequent-char? #\-) => #t)    ; 显式符号
(check (subsequent-char? #\.) => #t)    ; 显式符号
(check (subsequent-char? #\@) => #t)    ; 显式符号

;; 测试非 Subsequent 字符
(check (subsequent-char? #\() => #f)
(check (subsequent-char? #\)) => #f)
(check (subsequent-char? #\") => #f)
(check (subsequent-char? #\;) => #f)

;; ============================================
;; 测试特殊标识符判断
;; ============================================

(check (peculiar-identifier? "+") => #t)
(check (peculiar-identifier? "-") => #t)
(check (peculiar-identifier? "...") => #t)
(check (peculiar-identifier? "->") => #t)
(check (peculiar-identifier? "->foo") => #t)
(check (peculiar-identifier? "+1") => #t)
(check (peculiar-identifier? "-abc") => #t)

(check (peculiar-identifier? "foo") => #f)
(check (peculiar-identifier? "1+") => #f)
(check (peculiar-identifier? "a->") => #f)

;; ============================================
;; 测试标识符读取集成测试
;; ============================================

;; 测试普通标识符（包含WHITESPACE token）
(let ((lexer (make-lexer "define if+ list->vector")))
  ;; 标识符 "define"
  (let ((token1 (lexer-next-token lexer)))
    (check (identifier-token? token1) => #t)
    (check (token-lexeme token1) => "define"))
  ;; 第一个空格
  (let ((ws1 (lexer-next-token lexer)))
    (check (whitespace-token? ws1) => #t)
    (check (token-lexeme ws1) => " "))
  ;; 标识符 "if+"
  (let ((token2 (lexer-next-token lexer)))
    (check (identifier-token? token2) => #t)
    (check (token-lexeme token2) => "if+"))
  ;; 第二个空格
  (let ((ws2 (lexer-next-token lexer)))
    (check (whitespace-token? ws2) => #t)
    (check (token-lexeme ws2) => " "))
  ;; 标识符 "list->vector"
  (let ((token3 (lexer-next-token lexer)))
    (check (identifier-token? token3) => #t)
    (check (token-lexeme token3) => "list->vector")))

;; 测试特殊标识符（包含WHITESPACE token）
(let ((lexer (make-lexer "+ - ... -> +1 -abc")))
  ;; 标识符 "+"
  (let ((token1 (lexer-next-token lexer)))
    (check (identifier-token? token1) => #t)
    (check (token-lexeme token1) => "+"))
  ;; 空格1
  (let ((ws1 (lexer-next-token lexer)))
    (check (whitespace-token? ws1) => #t)
    (check (token-lexeme ws1) => " "))
  ;; 标识符 "-"
  (let ((token2 (lexer-next-token lexer)))
    (check (identifier-token? token2) => #t)
    (check (token-lexeme token2) => "-"))
  ;; 空格2
  (let ((ws2 (lexer-next-token lexer)))
    (check (whitespace-token? ws2) => #t)
    (check (token-lexeme ws2) => " "))
  ;; 标识符 "..."
  (let ((token3 (lexer-next-token lexer)))
    (check (identifier-token? token3) => #t)
    (check (token-lexeme token3) => "..."))
  ;; 空格3
  (let ((ws3 (lexer-next-token lexer)))
    (check (whitespace-token? ws3) => #t)
    (check (token-lexeme ws3) => " "))
  ;; 标识符 "->"
  (let ((token4 (lexer-next-token lexer)))
    (check (identifier-token? token4) => #t)
    (check (token-lexeme token4) => "->"))
  ;; 空格4
  (let ((ws4 (lexer-next-token lexer)))
    (check (whitespace-token? ws4) => #t)
    (check (token-lexeme ws4) => " "))
  ;; 标识符 "+1"
  (let ((token5 (lexer-next-token lexer)))
    (check (identifier-token? token5) => #t)
    (check (token-lexeme token5) => "+1"))
  ;; 空格5
  (let ((ws5 (lexer-next-token lexer)))
    (check (whitespace-token? ws5) => #t)
    (check (token-lexeme ws5) => " "))
  ;; 标识符 "-abc"
  (let ((token6 (lexer-next-token lexer)))
    (check (identifier-token? token6) => #t)
    (check (token-lexeme token6) => "-abc")))

;; 测试数字优先规则（包含WHITESPACE token）
(let ((lexer (make-lexer "123 abc 456")))
  ;; 第一个数字
  (let ((token1 (lexer-next-token lexer)))
    (check (number-token? token1) => #t)
    (check (token-lexeme token1) => "123")
    (check (token-value token1) => 123))
  ;; 第一个空格
  (let ((ws1 (lexer-next-token lexer)))
    (check (whitespace-token? ws1) => #t)
    (check (token-lexeme ws1) => " "))
  ;; 标识符
  (let ((token2 (lexer-next-token lexer)))
    (check (identifier-token? token2) => #t)
    (check (token-lexeme token2) => "abc"))
  ;; 第二个空格
  (let ((ws2 (lexer-next-token lexer)))
    (check (whitespace-token? ws2) => #t)
    (check (token-lexeme ws2) => " "))
  ;; 第二个数字
  (let ((token3 (lexer-next-token lexer)))
    (check (number-token? token3) => #t)
    (check (token-lexeme token3) => "456")
    (check (token-value token3) => 456)))

;; 测试点号处理（包含WHITESPACE token）
(let ((lexer (make-lexer ".foo . 123")))
  ;; 标识符 ".foo"
  (let ((token1 (lexer-next-token lexer)))
    (check (identifier-token? token1) => #t)
    (check (token-lexeme token1) => ".foo"))
  ;; 第一个空格
  (let ((ws1 (lexer-next-token lexer)))
    (check (whitespace-token? ws1) => #t)
    (check (token-lexeme ws1) => " "))
  ;; 单独的 . 应该由主 lexer 处理，不是标识符
  (let ((token2 (lexer-next-token lexer)))
    (check (token-type token2) => 'ERROR))
  ;; 第二个空格
  (let ((ws2 (lexer-next-token lexer)))
    (check (whitespace-token? ws2) => #t)
    (check (token-lexeme ws2) => " "))
  ;; 数字 "123"
  (let ((token3 (lexer-next-token lexer)))
    (check (number-token? token3) => #t)
    (check (token-lexeme token3) => "123")))

;; 注意：Unicode 标识符测试暂时跳过，取决于 Goldfish Scheme 的具体实现
;; 和源代码文件的编码设置

(check-report "Goldfix 标识符词法分析模块测试完成")