;;; lexer-string.scm - Goldfix 字符串词法分析器模块

(define-library (goldfix lexer-string)
  (export read-string
          string-token?)

  (import (scheme base)
          (scheme char)
          (goldfix lexer-base))

  (begin
    ;; ============================================
    ;; 字符串 Token 判断函数
    ;; ============================================

    (define (string-token? token)
      (eq? (token-type token) 'STRING))

    ;; ============================================
    ;; 字符串读取函数
    ;; ============================================

    ;; 读取字符串字面量（"hello", "world\n", "escaped \"quote\"" 等）
    (define (read-string lexer)
      (let ((start-line (lexer-line lexer))
            (start-column (lexer-column lexer))
            (start-offset (lexer-offset lexer))
            (start-indent (lexer-indent lexer))
            (start-leading-ws (lexer-leading-ws lexer))
            (start-pos (lexer-position lexer)))

        ;; 跳过起始的双引号
        (next-char! lexer)  ; 跳过 "

        ;; 收集字符串内容
        (let loop ((chars '())
                   (in-escape #f)
                   (terminated #f)
                   (has-error #f))
          (let ((ch (current-char lexer)))
            (cond
             ;; 遇到文件结束（未终止的字符串）
             ((not ch)
              (let ((lexeme (substring (lexer-source lexer)
                                       start-pos
                                       (lexer-position lexer))))
                (make-token 'ERROR
                           lexeme
                           start-line
                           start-column
                           start-offset
                           start-indent
                           #f
                           start-leading-ws
                           #f   ; 未终止
                           #t))) ; 有错误

             ;; 遇到转义字符
             ((and (not in-escape) (char=? ch #\\))
              (next-char! lexer)
              (let ((next-ch (current-char lexer)))
                (cond
                 ;; 转义双引号
                 ((and next-ch (char=? next-ch #\"))
                  (next-char! lexer)
                  (loop (cons #\" chars) #f terminated has-error))
                 ;; 其他转义字符（暂时不支持，按原样处理）
                 (next-ch
                  (next-char! lexer)
                  (loop (cons next-ch chars) #f terminated has-error))
                 ;; 转义字符后面没有字符（文件结束）
                 (else
                  (loop chars #f terminated #t))))) ; 标记有错误

             ;; 遇到双引号（字符串结束）
             ((and (not in-escape) (char=? ch #\"))
              (next-char! lexer)
              (let ((lexeme (substring (lexer-source lexer)
                                       start-pos
                                       (lexer-position lexer)))
                    (value (list->string (reverse chars))))
                (make-token 'STRING
                           lexeme
                           start-line
                           start-column
                           start-offset
                           start-indent
                           value
                           start-leading-ws
                           #t   ; 已终止
                           has-error)))

             ;; 遇到换行符（字符串不跨行）
             ((and (not in-escape) (char=? ch #\newline))
              ;; 字符串遇到换行符，提前结束（未终止的字符串）
              (let ((lexeme (substring (lexer-source lexer)
                                       start-pos
                                       (lexer-position lexer))))
                (make-token 'ERROR
                           lexeme
                           start-line
                           start-column
                           start-offset
                           start-indent
                           #f
                           start-leading-ws
                           #f   ; 未终止
                           #t))) ; 有错误

             ;; 其他字符
             (else
              (next-char! lexer)
              (loop (cons ch chars) #f terminated has-error)))))))

    ;; ============================================
    ;; 辅助函数（如果需要可以添加）
    ;; ============================================

  ) ; end of begin
) ; end of define-library