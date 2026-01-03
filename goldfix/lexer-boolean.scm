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

    ;; 读取布尔值（#t, #f, #true, #false）
    (define (read-boolean lexer)
      (let ((start-line (lexer-line lexer))
            (start-column (lexer-column lexer))
            (start-offset (lexer-offset lexer))
            (start-indent (lexer-indent lexer))
            (start-leading-ws (lexer-leading-ws lexer))
            (start-pos (lexer-position lexer)))

        ;; 检查是否有 # 前缀（应该已经检查过了）
        (next-char! lexer)  ; 跳过 #
        (let ((first-char (current-char lexer)))
          (cond
           ((and first-char (char=? first-char #\t))
            ;; 可能是 #t 或 #true
            (next-char! lexer)  ; 跳过 t
            (let ((next-char (current-char lexer)))
              (if (and next-char (char=? next-char #\r))
                  ;; 尝试读取 #true
                  (read-true lexer start-line start-column start-offset
                            start-indent start-leading-ws start-pos)
                  ;; 只是 #t
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
                               #f))))) ; has-error?
           ((and first-char (char=? first-char #\f))
            ;; 可能是 #f 或 #false
            (next-char! lexer)  ; 跳过 f
            (let ((next-char (current-char lexer)))
              (if (and next-char (char=? next-char #\a))
                  ;; 尝试读取 #false
                  (read-false lexer start-line start-column start-offset
                             start-indent start-leading-ws start-pos)
                  ;; 只是 #f
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
                               #f))))) ; has-error?
           (else
            ;; 无效的布尔值字符，生成错误 token
            (let ((lexeme (if first-char
                              (string #\# first-char)
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

    ;; 读取 #true
    (define (read-true lexer start-line start-column start-offset
                      start-indent start-leading-ws start-pos)
      ;; 已经读取了 #t，现在检查是否是 #true
      (let ((expected-chars '(#\r #\u #\e))
            (saved-position (lexer-position lexer)))  ; 保存当前位置
        (let loop ((chars expected-chars))
          (if (null? chars)
              ;; 成功读取了 #true
              (let ((lexeme (substring (lexer-source lexer)
                                       start-pos
                                       (lexer-position lexer))))
                (make-token 'BOOLEAN
                           lexeme
                           start-line
                           start-column
                           start-offset
                           start-indent
                           #t   ; literal value: #true
                           start-leading-ws
                           #t   ; is-terminated?
                           #f))
              (let ((ch (current-char lexer)))
                (if (and ch (char=? ch (car chars)))
                    (begin
                      (next-char! lexer)
                      (loop (cdr chars)))
                    ;; 不是完整的 #true，回退到 #t
                    ;; 重置 lexer 位置到只读取了 #t 的位置
                    (begin
                      (set-lexer-position! lexer saved-position)
                      (let ((lexeme (substring (lexer-source lexer)
                                               start-pos
                                               saved-position)))
                        (make-token 'BOOLEAN
                                   lexeme
                                   start-line
                                   start-column
                                   start-offset
                                   start-indent
                                   #t   ; literal value: #t
                                   start-leading-ws
                                   #t   ; is-terminated?
                                   #f)))))))))

    ;; 读取 #false
    (define (read-false lexer start-line start-column start-offset
                       start-indent start-leading-ws start-pos)
      ;; 已经读取了 #f，现在检查是否是 #false
      (let ((expected-chars '(#\a #\l #\s #\e))
            (saved-position (lexer-position lexer)))  ; 保存当前位置
        (let loop ((chars expected-chars))
          (if (null? chars)
              ;; 成功读取了 #false
              (let ((lexeme (substring (lexer-source lexer)
                                       start-pos
                                       (lexer-position lexer))))
                (make-token 'BOOLEAN
                           lexeme
                           start-line
                           start-column
                           start-offset
                           start-indent
                           #f   ; literal value: #false
                           start-leading-ws
                           #t   ; is-terminated?
                           #f))
              (let ((ch (current-char lexer)))
                (if (and ch (char=? ch (car chars)))
                    (begin
                      (next-char! lexer)
                      (loop (cdr chars)))
                    ;; 不是完整的 #false，回退到 #f
                    ;; 重置 lexer 位置到只读取了 #f 的位置
                    (begin
                      (set-lexer-position! lexer saved-position)
                      (let ((lexeme (substring (lexer-source lexer)
                                               start-pos
                                               saved-position)))
                        (make-token 'BOOLEAN
                                   lexeme
                                   start-line
                                   start-column
                                   start-offset
                                   start-indent
                                   #f   ; literal value: #f
                                   start-leading-ws
                                   #t   ; is-terminated?
                                   #f)))))))))

  ) ; end of begin
) ; end of define-library