;;; lexer.scm - Goldfix 词法分析器模块

(define-library (goldfix lexer)
  (export hello)

  (import (scheme base))

  (begin
    ;; 返回 "world" 字符串的 hello 函数
    (define (hello)
      "world")))