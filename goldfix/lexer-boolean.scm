;;; lexer-boolean.scm - Goldfix 布尔值词法分析器模块

(define-library (goldfix lexer-boolean)
  (export read-boolean)

  (import (scheme base)
          (scheme char)
          (goldfix lexer-base))

  (begin
    ;; ============================================
    ;; 布尔值读取函数
    ;; ============================================

    ;; 读取布尔值（#t, #T, #f, #F）
    (define (read-boolean lexer)
      (let ((start-line (lexer-line lexer))
            (start-column (lexer-column lexer))
            (start-offset (lexer-offset lexer))
            (start-indent (lexer-indent lexer))
            (start-leading-ws (lexer-leading-ws lexer))
            (start-pos (lexer-position lexer)))

        ;; 检查是否有 # 前缀（应该已经检查过了）
        (next-char! lexer)  ; 跳过 #
        (let ((bool-char (current-char lexer)))
          (cond
           ((and bool-char (or (char=? bool-char #\t) (char=? bool-char #\T)))
            (next-char! lexer)  ; 跳过 t/T
            (let ((lexeme (substring (lexer-source lexer)
                                     start-pos
                                     (lexer-position lexer))))
              (make-token 'BOOLEAN
                         lexeme
                         start-line
                         start-column
                         start-offset
                         start-indent
                         #t   ; literal value: #t
                         start-leading-ws
                         #t   ; is-terminated?
                         #f))) ; has-error?
           ((and bool-char (or (char=? bool-char #\f) (char=? bool-char #\F)))
            (next-char! lexer)  ; 跳过 f/F
            (let ((lexeme (substring (lexer-source lexer)
                                     start-pos
                                     (lexer-position lexer))))
              (make-token 'BOOLEAN
                         lexeme
                         start-line
                         start-column
                         start-offset
                         start-indent
                         #f   ; literal value: #f
                         start-leading-ws
                         #t   ; is-terminated?
                         #f))) ; has-error?
           (else
            ;; 无效的布尔值字符，生成错误 token
            (let ((lexeme (if bool-char
                              (string #\# bool-char)
                              "#")))
              (make-token 'ERROR
                         lexeme
                         start-line
                         start-column
                         start-offset
                         start-indent
                         #f
                         start-leading-ws
                         #t
                         #t))))))) ; has-error?

  ) ; end of begin
) ; end of define-library