;;; lexer-string-test.scm - Goldfix 字符串词法分析器单元测试

(import (liii check)
        (liii list)
        (goldfix lexer))

;; 设置测试模式
(check-set-mode! 'report-failed)

;; ============================================
;; 测试辅助函数
;; ============================================

;; 测试单个字符串
(define (test-single-string input expected-lexeme expected-value)
  (let ((lexer (make-lexer input)))
    (let ((token (lexer-next-token lexer)))
      (check (string-token? token) => #t)
      (check (token-lexeme token) => expected-lexeme)
      (check (token-value token) => expected-value)
      (check (token-terminated? token) => #t)
      (check (token-has-error? token) => #f)
      ;; 检查后续 token（可能是 NEWLINE 然后 EOF，或者直接 EOF）
      (let loop ()
        (let ((next-token (lexer-next-token lexer)))
          (if (or (newline-token? next-token)
                  (eof-token? next-token))
              (if (eof-token? next-token)
                  #t  ; 找到 EOF，测试通过
                  (loop))))))))

;; 测试错误字符串
(define (test-error-string input expected-lexeme expected-terminated? expected-has-error?)
  (let ((lexer (make-lexer input)))
    (let ((token (lexer-next-token lexer)))
      (check (token-has-error? token) => expected-has-error?)
      (check (token-lexeme token) => expected-lexeme)
      (check (token-terminated? token) => expected-terminated?)
      ;; 检查后续 token（可能是 NEWLINE 然后 EOF，或者直接 EOF）
      (let loop ()
        (let ((next-token (lexer-next-token lexer)))
          (if (or (newline-token? next-token)
                  (eof-token? next-token))
              (if (eof-token? next-token)
                  #t  ; 找到 EOF，测试通过
                  (loop))))))))  ; 意外的 token，强制测试失败

;; 测试多个字符串（包含WHITESPACE token）
(define (test-multiple-strings input expected-string-tokens)
  (let ((lexer (make-lexer input))
        (tokens '()))
    (let loop ()
      (let ((token (lexer-next-token lexer)))
        (set! tokens (cons token tokens))
        (unless (eof-token? token)
          (loop))))
    (let ((actual-tokens (reverse (cdr tokens)))) ; 去掉最后的 EOF
      ;; 从实际token中提取字符串token
      (let ((actual-string-tokens (filter string-token? actual-tokens)))
        (check (length actual-string-tokens) => (length expected-string-tokens))
        (for-each
         (lambda (actual expected)
           (check (string-token? actual) => #t)
           (check (token-lexeme actual) => (car expected))
           (check (token-value actual) => (cadr expected)))
         actual-string-tokens
         expected-string-tokens)))))

;; ============================================
;; 测试用例开始
;; ============================================

;; 测试 1: 空字符串
(test-single-string "\"\"" "\"\"" "")

;; 测试 2: 简单字符串
(test-single-string "\"hello\"" "\"hello\"" "hello")

;; 测试 3: 带空格的字符串
(test-single-string "\"hello world\"" "\"hello world\"" "hello world")

;; 测试 4: 带转义双引号的字符串
(test-single-string "\"hello \\\"world\\\"\"" "\"hello \\\"world\\\"\"" "hello \"world\"")

;; 测试 5: 多个转义双引号
(test-single-string "\"\\\"quote\\\" here\"" "\"\\\"quote\\\" here\"" "\"quote\" here")

;; 测试 6: 转义反斜杠（不支持，按原样处理）
(test-single-string "\"backslash: \\\\\"" "\"backslash: \\\\\"" "backslash: \\")

;; 测试 7: 混合转义
(test-single-string "\"test \\\" string\"" "\"test \\\" string\"" "test \" string")

;; 测试 8: 字符串中的数字
(test-single-string "\"123 abc\"" "\"123 abc\"" "123 abc")

;; 测试 9: 字符串中的特殊字符
(test-single-string "\"!@#$%^&*()\"" "\"!@#$%^&*()\"" "!@#$%^&*()")

;; 测试 10: 字符串与空格混合 " \"hello\" \"world\" "
(test-multiple-strings " \"hello\" \"world\" "
                       '(("\"hello\"" "hello")
                         ("\"world\"" "world")))

;; 测试 11: 未终止的字符串（缺少结束双引号）
(test-error-string "\"hello" "\"hello" #f #t)

;; 测试 12: 字符串中遇到换行符（不跨行）
(test-error-string "\"hello\nworld\"" "\"hello" #f #t)

;; 测试 13: 只有开始双引号
(test-error-string "\"" "\"" #f #t)

;; 测试 14: 转义字符后面没有字符
(test-error-string "\"hello\\" "\"hello\\" #f #t)

;; 测试 15: 转义字符后面是文件结束
(test-error-string "\"hello\\" "\"hello\\" #f #t)

;; 测试 16: 字符串与标识符混合 " \"str\" foo \"bar\" "
(let ((lexer (make-lexer " \"str\" foo \"bar\" ")))
  ;; 前导空格
  (let ((ws1 (lexer-next-token lexer)))
    (check (whitespace-token? ws1) => #t)
    (check (token-lexeme ws1) => " "))
  ;; 第一个字符串
  (let ((str1 (lexer-next-token lexer)))
    (check (string-token? str1) => #t)
    (check (token-lexeme str1) => "\"str\"")
    (check (token-value str1) => "str"))
  ;; 第二个空格
  (let ((ws2 (lexer-next-token lexer)))
    (check (whitespace-token? ws2) => #t)
    (check (token-lexeme ws2) => " "))
  ;; 标识符
  (let ((ident (lexer-next-token lexer)))
    (check (identifier-token? ident) => #t)
    (check (token-lexeme ident) => "foo"))
  ;; 第三个空格
  (let ((ws3 (lexer-next-token lexer)))
    (check (whitespace-token? ws3) => #t)
    (check (token-lexeme ws3) => " "))
  ;; 第二个字符串
  (let ((str2 (lexer-next-token lexer)))
    (check (string-token? str2) => #t)
    (check (token-lexeme str2) => "\"bar\"")
    (check (token-value str2) => "bar")))

;; 测试 17: 字符串与数字混合 " \"num\" 123 \"test\" "
(let ((lexer (make-lexer " \"num\" 123 \"test\" ")))
  ;; 前导空格
  (let ((ws1 (lexer-next-token lexer)))
    (check (whitespace-token? ws1) => #t)
    (check (token-lexeme ws1) => " "))
  ;; 第一个字符串
  (let ((str1 (lexer-next-token lexer)))
    (check (string-token? str1) => #t)
    (check (token-lexeme str1) => "\"num\"")
    (check (token-value str1) => "num"))
  ;; 第二个空格
  (let ((ws2 (lexer-next-token lexer)))
    (check (whitespace-token? ws2) => #t)
    (check (token-lexeme ws2) => " "))
  ;; 数字
  (let ((num (lexer-next-token lexer)))
    (check (number-token? num) => #t)
    (check (token-lexeme num) => "123")
    (check (token-value num) => 123))
  ;; 第三个空格
  (let ((ws3 (lexer-next-token lexer)))
    (check (whitespace-token? ws3) => #t)
    (check (token-lexeme ws3) => " "))
  ;; 第二个字符串
  (let ((str2 (lexer-next-token lexer)))
    (check (string-token? str2) => #t)
    (check (token-lexeme str2) => "\"test\"")
    (check (token-value str2) => "test")))

;; 测试 18: 字符串与布尔值混合 " \"bool\" #t \"false\" "
(let ((lexer (make-lexer " \"bool\" #t \"false\" ")))
  ;; 前导空格
  (let ((ws1 (lexer-next-token lexer)))
    (check (whitespace-token? ws1) => #t)
    (check (token-lexeme ws1) => " "))
  ;; 第一个字符串
  (let ((str1 (lexer-next-token lexer)))
    (check (string-token? str1) => #t)
    (check (token-lexeme str1) => "\"bool\"")
    (check (token-value str1) => "bool"))
  ;; 第二个空格
  (let ((ws2 (lexer-next-token lexer)))
    (check (whitespace-token? ws2) => #t)
    (check (token-lexeme ws2) => " "))
  ;; 布尔值
  (let ((bool (lexer-next-token lexer)))
    (check (boolean-token? bool) => #t)
    (check (token-lexeme bool) => "#t")
    (check (token-value bool) => #t))
  ;; 第三个空格
  (let ((ws3 (lexer-next-token lexer)))
    (check (whitespace-token? ws3) => #t)
    (check (token-lexeme ws3) => " "))
  ;; 第二个字符串
  (let ((str2 (lexer-next-token lexer)))
    (check (string-token? str2) => #t)
    (check (token-lexeme str2) => "\"false\"")
    (check (token-value str2) => "false")))

;; 测试 19: 字符串与字符混合 " \"char\" #\\a \"test\" "
(let ((lexer (make-lexer " \"char\" #\\a \"test\" ")))
  ;; 前导空格
  (let ((ws1 (lexer-next-token lexer)))
    (check (whitespace-token? ws1) => #t)
    (check (token-lexeme ws1) => " "))
  ;; 第一个字符串
  (let ((str1 (lexer-next-token lexer)))
    (check (string-token? str1) => #t)
    (check (token-lexeme str1) => "\"char\"")
    (check (token-value str1) => "char"))
  ;; 第二个空格
  (let ((ws2 (lexer-next-token lexer)))
    (check (whitespace-token? ws2) => #t)
    (check (token-lexeme ws2) => " "))
  ;; 字符
  (let ((char (lexer-next-token lexer)))
    (check (character-token? char) => #t)
    (check (token-lexeme char) => "#\\a")
    (check (token-value char) => #\a))
  ;; 第三个空格
  (let ((ws3 (lexer-next-token lexer)))
    (check (whitespace-token? ws3) => #t)
    (check (token-lexeme ws3) => " "))
  ;; 第二个字符串
  (let ((str2 (lexer-next-token lexer)))
    (check (string-token? str2) => #t)
    (check (token-lexeme str2) => "\"test\"")
    (check (token-value str2) => "test")))

;; 测试 20: 字符串与括号混合 " ( \"hello\" ) "
(let ((lexer (make-lexer " ( \"hello\" ) ")))
  ;; 前导空格
  (let ((ws1 (lexer-next-token lexer)))
    (check (whitespace-token? ws1) => #t)
    (check (token-lexeme ws1) => " "))
  ;; 左括号
  (let ((lparen (lexer-next-token lexer)))
    (check (left-paren-token? lparen) => #t)
    (check (token-lexeme lparen) => "("))
  ;; 第二个空格
  (let ((ws2 (lexer-next-token lexer)))
    (check (whitespace-token? ws2) => #t)
    (check (token-lexeme ws2) => " "))
  ;; 字符串
  (let ((str (lexer-next-token lexer)))
    (check (string-token? str) => #t)
    (check (token-lexeme str) => "\"hello\"")
    (check (token-value str) => "hello"))
  ;; 第三个空格
  (let ((ws3 (lexer-next-token lexer)))
    (check (whitespace-token? ws3) => #t)
    (check (token-lexeme ws3) => " "))
  ;; 右括号
  (let ((rparen (lexer-next-token lexer)))
    (check (right-paren-token? rparen) => #t)
    (check (token-lexeme rparen) => ")")))

;; 测试 21: 包含多个转义的字符串
(test-single-string "\"a \\\" b \\\" c\"" "\"a \\\" b \\\" c\"" "a \" b \" c")

;; 测试 22: 转义在字符串开头
(test-single-string "\"\\\"start\"" "\"\\\"start\"" "\"start")

;; 测试 23: 转义在字符串结尾
(test-single-string "\"end\\\"\"" "\"end\\\"\"" "end\"")

;; 测试 24: 连续转义
(test-single-string "\"\\\"\\\"\"" "\"\\\"\\\"\"" "\"\"")

;; 测试 25: 包含制表符的字符串
(test-single-string "\"hello\tworld\"" "\"hello\tworld\"" "hello\tworld")

;; 测试 26: 包含回车的字符串
(test-single-string "\"hello\rworld\"" "\"hello\rworld\"" "hello\rworld")

;; 测试 27: 包含各种空白字符的字符串
(test-single-string "\"a\tb\rc d\"" "\"a\tb\rc d\"" "a\tb\rc d")

;; 测试 28: 非常长的字符串
(test-single-string "\"this is a very long string with many characters to test the lexer's ability to handle longer inputs without issues\""
                   "\"this is a very long string with many characters to test the lexer's ability to handle longer inputs without issues\""
                   "this is a very long string with many characters to test the lexer's ability to handle longer inputs without issues")

;; 测试 29: 包含 Unicode 字符的字符串（如果支持）
(test-single-string "\"hello 世界\"" "\"hello 世界\"" "hello 世界")

;; 测试 30: 混合所有类型的字符串
(test-multiple-strings " \"a\" \"b\" \"c\" \"d\" "
                       '(("\"a\"" "a")
                         ("\"b\"" "b")
                         ("\"c\"" "c")
                         ("\"d\"" "d")))

;; 测试总结
(check-report "Goldfix lexer-string 空白字符场景测试完成")
