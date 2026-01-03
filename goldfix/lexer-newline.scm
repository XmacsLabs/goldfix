;;; lexer-newline.scm - Goldfix 换行符词法分析器模块

(define-library (goldfix lexer-newline)
  (export read-newline)

  (import (scheme base)
          (scheme char)
          (goldfix lexer-base))

  (begin
    ;; ============================================
    ;; 换行符读取函数
    ;; ============================================

    ;; 读取换行符
    (define (read-newline lexer)
      (let ((ch (current-char lexer)))
        (if (and ch (char=? ch #\newline))
            (let ((lexeme (string ch))
                  (start-line (lexer-line lexer))
                  (start-column (lexer-column lexer))
                  (start-offset (lexer-offset lexer))
                  (start-indent (lexer-indent lexer))
                  (start-leading-ws (lexer-leading-ws lexer)))
              (next-char! lexer)  ; 跳过换行符
              ;; 重置前导空格，因为新的一行开始了
              (set-lexer-leading-ws! lexer "")
              ;; 返回 NEWLINE token
              (make-token 'NEWLINE
                         lexeme
                         start-line
                         start-column
                         start-offset
                         start-indent
                         #f
                         start-leading-ws
                         #t
                         #f))
            #f)))  ; 不是换行符，返回 #f

  ) ; end of begin
) ; end of define-library