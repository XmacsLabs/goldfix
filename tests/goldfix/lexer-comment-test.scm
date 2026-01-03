;;; lexer-comment-test.scm - Goldfix 注释词法分析器单元测试

(import (liii check)
        (liii list)
        (goldfix lexer))

;; 设置测试模式
(check-set-mode! 'report-failed)

;; ============================================
;; 测试辅助函数
;; ============================================

;; 测试单个注释
(define (test-single-comment input expected-lexeme)
  (let ((lexer (make-lexer input)))
    (let ((token (lexer-next-token lexer)))
      (check (comment-token? token) => #t)
      (check (token-lexeme token) => expected-lexeme)
      (check (token-value token) => #f)
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


;; ============================================
;; 测试用例开始
;; ============================================

;; 测试 1: 简单注释 ;
(test-single-comment ";" ";")

;; 测试 2: 带空格的注释 ; comment
(test-single-comment "; comment" "; comment")

;; 测试 3: 带多个空格的注释 ;   comment   with   spaces
(test-single-comment ";   comment   with   spaces" ";   comment   with   spaces")

;; 测试 4: 包含特殊字符的注释 ; !@#$%^&*()
(test-single-comment "; !@#$%^&*()" "; !@#$%^&*()")

;; 测试 5: 包含括号的注释 ; (define x 1)
(test-single-comment "; (define x 1)" "; (define x 1)")

;; 测试 6: 包含数字的注释 ; 123 abc
(test-single-comment "; 123 abc" "; 123 abc")

;; 测试 7: 包含标识符的注释 ; define
(test-single-comment "; define" "; define")

;; 测试 8: 包含布尔值的注释 ; #t #f
(test-single-comment "; #t #f" "; #t #f")

;; 测试 9: 包含字符的注释 ; #\a
(test-single-comment "; #\\a" "; #\\a")

;; 测试 10: 包含字符串的注释 ; "hello"
(test-single-comment "; \"hello\"" "; \"hello\"")

;; 测试 11: 多个注释 " ;a ;b ;c"（包含WHITESPACE token）
;; 注意：注释读取到行尾，所以 ";a ;b ;c" 是一个注释
(test-single-comment ";a ;b ;c" ";a ;b ;c")

;; 测试 12: 注释与空格混合 " ;a  ;b   ;c"
;; 注意：注释读取到行尾，所以 ";a  ;b   ;c" 是一个注释
(test-single-comment ";a  ;b   ;c" ";a  ;b   ;c")

;; 测试 13: 注释与标识符混合 " ;comment foo ;bar"
;; 注意：注释读取到行尾，所以 ";comment foo ;bar" 是一个注释
(let ((lexer (make-lexer " ;comment foo ;bar")))
  ;; 前导空格
  (let ((ws1 (lexer-next-token lexer)))
    (check (whitespace-token? ws1) => #t)
    (check (token-lexeme ws1) => " "))
  ;; 注释（包含整个 ";comment foo ;bar"）
  (let ((comment (lexer-next-token lexer)))
    (check (comment-token? comment) => #t)
    (check (token-lexeme comment) => ";comment foo ;bar"))
  ;; EOF
  (let ((eof (lexer-next-token lexer)))
    (check (eof-token? eof) => #t)))

;; 测试 14: 注释与数字混合 " ;num 123 ;456"
;; 注意：注释读取到行尾，所以 ";num 123 ;456" 是一个注释
(let ((lexer (make-lexer " ;num 123 ;456")))
  ;; 前导空格
  (let ((ws1 (lexer-next-token lexer)))
    (check (whitespace-token? ws1) => #t)
    (check (token-lexeme ws1) => " "))
  ;; 注释（包含整个 ";num 123 ;456"）
  (let ((comment (lexer-next-token lexer)))
    (check (comment-token? comment) => #t)
    (check (token-lexeme comment) => ";num 123 ;456"))
  ;; EOF
  (let ((eof (lexer-next-token lexer)))
    (check (eof-token? eof) => #t)))

;; 测试 15: 注释与布尔值混合 " ;bool #t ;false"
;; 注意：注释读取到行尾，所以 ";bool #t ;false" 是一个注释
(let ((lexer (make-lexer " ;bool #t ;false")))
  ;; 前导空格
  (let ((ws1 (lexer-next-token lexer)))
    (check (whitespace-token? ws1) => #t)
    (check (token-lexeme ws1) => " "))
  ;; 注释（包含整个 ";bool #t ;false"）
  (let ((comment (lexer-next-token lexer)))
    (check (comment-token? comment) => #t)
    (check (token-lexeme comment) => ";bool #t ;false"))
  ;; EOF
  (let ((eof (lexer-next-token lexer)))
    (check (eof-token? eof) => #t)))

;; 测试 19: 注释在行尾（后面是换行符）
(let ((lexer (make-lexer "; comment\n")))
  ;; 注释
  (let ((comment (lexer-next-token lexer)))
    (check (comment-token? comment) => #t)
    (check (token-lexeme comment) => "; comment"))
  ;; 换行符
  (let ((nl (lexer-next-token lexer)))
    (check (newline-token? nl) => #t)
    (check (token-lexeme nl) => "\n"))
  ;; EOF
  (let ((eof (lexer-next-token lexer)))
    (check (eof-token? eof) => #t)))

;; 测试 20: 多个注释和换行符混合
(let ((lexer (make-lexer ";a\n;b\n;c")))
  ;; 第一个注释
  (let ((comment1 (lexer-next-token lexer)))
    (check (comment-token? comment1) => #t)
    (check (token-lexeme comment1) => ";a"))
  ;; 第一个换行符
  (let ((nl1 (lexer-next-token lexer)))
    (check (newline-token? nl1) => #t)
    (check (token-lexeme nl1) => "\n"))
  ;; 第二个注释
  (let ((comment2 (lexer-next-token lexer)))
    (check (comment-token? comment2) => #t)
    (check (token-lexeme comment2) => ";b"))
  ;; 第二个换行符
  (let ((nl2 (lexer-next-token lexer)))
    (check (newline-token? nl2) => #t)
    (check (token-lexeme nl2) => "\n"))
  ;; 第三个注释
  (let ((comment3 (lexer-next-token lexer)))
    (check (comment-token? comment3) => #t)
    (check (token-lexeme comment3) => ";c"))
  ;; EOF
  (let ((eof (lexer-next-token lexer)))
    (check (eof-token? eof) => #t)))

;; 测试 21: 空注释（只有分号）
(test-single-comment ";" ";")

;; 测试 22: 注释包含制表符 ;\tcomment
(test-single-comment ";\tcomment" ";\tcomment")

;; 测试 23: 注释包含回车符 ;\rcomment
(test-single-comment ";\rcomment" ";\rcomment")

;; 测试 24: 注释包含各种空白字符 ; \t \r comment
(test-single-comment "; \t \r comment" "; \t \r comment")

;; 测试 25: 注释在文件末尾（没有换行符）
(test-single-comment "; comment" "; comment")

;; 测试 26: 注释与多个空格混合 "   ;comment"
(let ((lexer (make-lexer "   ;comment")))
  ;; 前导空格
  (let ((ws (lexer-next-token lexer)))
    (check (whitespace-token? ws) => #t)
    (check (token-lexeme ws) => "   "))
  ;; 注释
  (let ((comment (lexer-next-token lexer)))
    (check (comment-token? comment) => #t)
    (check (token-lexeme comment) => ";comment"))
  ;; EOF
  (let ((eof (lexer-next-token lexer)))
    (check (eof-token? eof) => #t)))

;; 测试 27: 注释与制表符混合 "\t;comment"
(let ((lexer (make-lexer "\t;comment")))
  ;; 前导制表符
  (let ((ws (lexer-next-token lexer)))
    (check (whitespace-token? ws) => #t)
    (check (token-lexeme ws) => "\t"))
  ;; 注释
  (let ((comment (lexer-next-token lexer)))
    (check (comment-token? comment) => #t)
    (check (token-lexeme comment) => ";comment"))
  ;; EOF
  (let ((eof (lexer-next-token lexer)))
    (check (eof-token? eof) => #t)))

;; 测试 28: 注释与换行符和空格混合 " ;a\n  ;b"
(let ((lexer (make-lexer " ;a\n  ;b")))
  ;; 前导空格
  (let ((ws1 (lexer-next-token lexer)))
    (check (whitespace-token? ws1) => #t)
    (check (token-lexeme ws1) => " "))
  ;; 第一个注释
  (let ((comment1 (lexer-next-token lexer)))
    (check (comment-token? comment1) => #t)
    (check (token-lexeme comment1) => ";a"))
  ;; 换行符
  (let ((nl (lexer-next-token lexer)))
    (check (newline-token? nl) => #t)
    (check (token-lexeme nl) => "\n"))
  ;; 第二行前导空格
  (let ((ws2 (lexer-next-token lexer)))
    (check (whitespace-token? ws2) => #t)
    (check (token-lexeme ws2) => "  "))
  ;; 第二个注释
  (let ((comment2 (lexer-next-token lexer)))
    (check (comment-token? comment2) => #t)
    (check (token-lexeme comment2) => ";b"))
  ;; EOF
  (let ((eof (lexer-next-token lexer)))
    (check (eof-token? eof) => #t)))

;; 测试 29: 注释包含 Unicode 字符（如果支持）
(test-single-comment "; hello 世界" "; hello 世界")

;; 测试 30: 非常长的注释
(test-single-comment "; this is a very long comment with many characters to test the lexer's ability to handle longer comments without issues"
                     "; this is a very long comment with many characters to test the lexer's ability to handle longer comments without issues")

;; 测试总结
(check-report "Goldfix lexer-comment 注释场景测试完成")