;;; lexer-number.scm - Goldfix 数字词法分析器模块

(define-library (goldfix lexer-number)
  (export lexer-next-token
          read-number
          read-radix-number
          read-decimal-number
          is-radix-digit?)

  (import (scheme base)
          (scheme char)
          (goldfix lexer-base))

  (begin
    ;; ============================================
    ;; 数字读取函数
    ;; ============================================

    ;; 读取数字（支持2、8、10、16进制）
    (define (read-number lexer)
      (let ((start-line (lexer-line lexer))
            (start-column (lexer-column lexer))
            (start-offset (lexer-offset lexer))
            (start-indent (lexer-indent lexer))
            (start-leading-ws (lexer-leading-ws lexer))
            (start-pos (lexer-position lexer)))

        ;; 检查是否有 # 前缀
        (let ((ch (current-char lexer)))
          (if (and ch (char=? ch #\#))
              ;; 有 # 前缀，检查进制
              (begin
                (next-char! lexer)  ; 跳过 #
                (let ((radix-char (current-char lexer)))
                  (cond
                   ((and radix-char (or (char=? radix-char #\b) (char=? radix-char #\B)))
                    (next-char! lexer)  ; 跳过 b/B
                    (read-radix-number lexer 2 start-line start-column start-offset
                                      start-indent start-leading-ws start-pos))
                   ((and radix-char (or (char=? radix-char #\o) (char=? radix-char #\O)))
                    (next-char! lexer)  ; 跳过 o/O
                    (read-radix-number lexer 8 start-line start-column start-offset
                                      start-indent start-leading-ws start-pos))
                   ((and radix-char (or (char=? radix-char #\d) (char=? radix-char #\D)))
                    (next-char! lexer)  ; 跳过 d/D
                    (read-decimal-number lexer start-line start-column start-offset
                                        start-indent start-leading-ws start-pos))
                   ((and radix-char (or (char=? radix-char #\x) (char=? radix-char #\X)))
                    (next-char! lexer)  ; 跳过 x/X
                    (read-radix-number lexer 16 start-line start-column start-offset
                                      start-indent start-leading-ws start-pos))
                   (else
                    ;; 无效的进制前缀，生成错误 token
                    (let ((lexeme (if radix-char
                                      (string #\# radix-char)
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
                                 #t))))))
              ;; 没有 # 前缀，按十进制处理（可能包含小数）
              (read-decimal-number lexer start-line start-column start-offset
                                  start-indent start-leading-ws start-pos)))))

    ;; ============================================
    ;; 进制数字读取函数
    ;; ============================================

    ;; 读取指定进制的数字（2、8、16进制）
    (define (read-radix-number lexer radix start-line start-column start-offset
                              start-indent start-leading-ws start-pos)
      (let loop ((has-digit? #f))
        (let ((ch (current-char lexer)))
          (cond
           ((and ch (is-radix-digit? ch radix))
            (next-char! lexer)
            (loop #t))
           (else
            (let ((lexeme (substring (lexer-source lexer)
                                     start-pos
                                     (lexer-position lexer))))
              (if has-digit?
                  ;; 有数字部分，尝试解析
                  (let ((value (string->number (substring lexeme 2) radix))) ; 跳过 #b/#o/#x 前缀
                    (make-token 'NUMBER
                               lexeme
                               start-line
                               start-column
                               start-offset
                               start-indent
                               value
                               start-leading-ws
                               #t   ; is-terminated?
                               #f))
                  ;; 没有数字部分，生成错误 token
                  (make-token 'ERROR
                             lexeme
                             start-line
                             start-column
                             start-offset
                             start-indent
                             #f
                             start-leading-ws
                             #t
                             #t)))))))) ; has-error?

    ;; 检查字符是否是指定进制的有效数字
    (define (is-radix-digit? ch radix)
      (case radix
        ((2)   (or (char=? ch #\0) (char=? ch #\1)))
        ((8)   (and (char>=? ch #\0) (char<=? ch #\7)))
        ((16)  (or (and (char>=? ch #\0) (char<=? ch #\9))
                   (and (char>=? ch #\a) (char<=? ch #\f))
                   (and (char>=? ch #\A) (char<=? ch #\F))))
        (else #f)))

    ;; 读取十进制数字（可能包含小数）
    (define (read-decimal-number lexer start-line start-column start-offset
                                start-indent start-leading-ws start-pos)
      (let loop ((has-digit? #f) (has-dot? #f))
        (let ((ch (current-char lexer)))
          (cond
           ((and ch (char-numeric? ch))
            (next-char! lexer)
            (loop #t has-dot?))
           ((and ch (char=? ch #\.) (not has-dot?))
            (next-char! lexer)
            (loop has-digit? #t))
           (else
            (let ((lexeme (substring (lexer-source lexer)
                                     start-pos
                                     (lexer-position lexer))))
              (if (or has-digit? has-dot?)
                  ;; 有数字或小数点，尝试解析
                  (let ((value (if (and (>= (string-length lexeme) 2)
                                        (char=? (string-ref lexeme 0) #\#)
                                        (or (char=? (string-ref lexeme 1) #\d)
                                            (char=? (string-ref lexeme 1) #\D)))
                                   ;; 有 #d 或 #D 前缀，跳过前缀
                                   (string->number (substring lexeme 2))
                                   ;; 没有前缀或无效前缀
                                   (string->number lexeme))))
                    (make-token 'NUMBER
                               lexeme
                               start-line
                               start-column
                               start-offset
                               start-indent
                               value
                               start-leading-ws
                               #t   ; is-terminated?
                               #f))
                  ;; 既没有数字也没有小数点，生成错误 token
                  (make-token 'ERROR
                             lexeme
                             start-line
                             start-column
                             start-offset
                             start-indent
                             #f
                             start-leading-ws
                             #t
                             #t)))))))) ; has-error?

    ;; ============================================
    ;; 主扫描函数
    ;; ============================================

    (define (lexer-next-token lexer)
      (skip-whitespace! lexer)
      (let ((ch (current-char lexer)))
        (cond
         ((not ch)
          (create-token lexer 'EOF "" #f))
         ((or (char-numeric? ch) (char=? ch #\#))
          (read-number lexer))
         (else
          ;; 对于非数字字符，生成错误 Token（简化处理）
          (let ((lexeme (string ch))
                (start-line (lexer-line lexer))
                (start-column (lexer-column lexer))
                (start-offset (lexer-offset lexer))
                (start-indent (lexer-indent lexer))
                (start-leading-ws (lexer-leading-ws lexer)))
            (next-char! lexer)
            (make-token 'ERROR
                       lexeme
                       start-line
                       start-column
                       start-offset
                       start-indent
                       #f
                       start-leading-ws
                       #t
                       #t))))))

  ) ; end of begin
) ; end of define-library