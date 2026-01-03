;;; lexer.scm - Goldfix 词法分析器主模块

(define-library (goldfix lexer)
  (export make-lexer
          lexer-next-token
          token-type
          token-lexeme
          token-line
          token-column
          token-offset
          token-indent
          token-value
          token-leading-ws
          token-terminated?
          token-has-error?
          token?
          eof-token?
          number-token?)

  (import (scheme base)
          (goldfix lexer-base)
          (goldfix lexer-number))

  (begin
    ;; 重新导出基础模块的函数
    ;; Token 相关函数
    (define token? token?)
    (define token-type token-type)
    (define token-lexeme token-lexeme)
    (define token-line token-line)
    (define token-column token-column)
    (define token-offset token-offset)
    (define token-indent token-indent)
    (define token-value token-value)
    (define token-leading-ws token-leading-ws)
    (define token-terminated? token-terminated?)
    (define token-has-error? token-has-error?)
    (define eof-token? eof-token?)
    (define number-token? number-token?)

    ;; Lexer 相关函数
    (define make-lexer make-lexer)
    (define lexer-next-token lexer-next-token)
  ) ; end of begin
) ; end of define-library