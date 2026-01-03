;;; lexer-character.scm - Goldfix 字符词法分析器模块

(define-library (goldfix lexer-character)
  (export read-character
          character-token?)

  (import (scheme base)
          (scheme char)
          (goldfix lexer-base))

  (begin
    ;; ============================================
    ;; 字符 Token 判断函数
    ;; ============================================

    (define (character-token? token)
      (eq? (token-type token) 'CHARACTER))

    ;; ============================================
    ;; 字符读取函数
    ;; ============================================

    ;; 读取字符字面量（#\a, #\newline, #\space, #\xNN 等）
    (define (read-character lexer)
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
           ((and first-char (char=? first-char #\\))
            ;; 开始读取字符
            (next-char! lexer)  ; 跳过 \
            (let ((char-char (current-char lexer)))
              (cond
               ((not char-char)
                ;; 只有 #\，没有字符
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
                             #t
                             #t)))
               ((char=? char-char #\x)
                ;; 十六进制字符： #\xNN (只支持小写 x)
                (read-hex-character lexer start-line start-column start-offset
                                   start-indent start-leading-ws start-pos))
               ((and (char-alphabetic? char-char) (char-ci=? char-char #\x))
                ;; 大写 X：warning - 只支持小写 x 作为十六进制前缀
                ;; 按照 Goldfish Scheme 行为：大写 X 被当作普通字符处理
                ;; 例如 #\X20 被解析为字符 #\X 和数字 20
                (read-named-character lexer start-line start-column start-offset
                                     start-indent start-leading-ws start-pos))
               ((char-alphabetic? char-char)
                ;; 可能是命名字符： #\newline, #\space 等
                (read-named-character lexer start-line start-column start-offset
                                     start-indent start-leading-ws start-pos))
               (else
                ;; 简单字符： #\a, #\1, #\+ 等
                (read-simple-character lexer start-line start-column start-offset
                                      start-indent start-leading-ws start-pos)))))
           (else
            ;; 无效的字符语法：应该有 \ 但没有
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
                         #t)))))))

    ;; 读取简单字符： #\a, #\1, #\+ 等
    (define (read-simple-character lexer start-line start-column start-offset
                                  start-indent start-leading-ws start-pos)
      (let ((char-char (current-char lexer)))
        (next-char! lexer)  ; 跳过字符
        (let ((lexeme (substring (lexer-source lexer)
                                 start-pos
                                 (lexer-position lexer))))
          (make-token 'CHARACTER
                     lexeme
                     start-line
                     start-column
                     start-offset
                     start-indent
                     char-char   ; literal value: 字符本身
                     start-leading-ws
                     #t   ; is-terminated?
                     #f)))) ; has-error?

    ;; 读取命名字符： #\newline, #\space, #\tab 等
    (define (read-named-character lexer start-line start-column start-offset
                                 start-indent start-leading-ws start-pos)
      ;; 收集字符名称
      (let loop ((name-chars '())
                 (saved-position (lexer-position lexer)))  ; 保存当前位置
        (let ((ch (current-char lexer)))
          (cond
           ((and ch (char-alphabetic? ch))
            (next-char! lexer)
            (loop (cons ch name-chars) (lexer-position lexer)))
           (else
            ;; 尝试匹配命名字符
            (let* ((name (list->string (reverse name-chars)))
                   (named-char (named-character->char name)))
              (if named-char
                  ;; 成功匹配命名字符
                  (let ((lexeme (substring (lexer-source lexer)
                                           start-pos
                                           (lexer-position lexer))))
                    (make-token 'CHARACTER
                               lexeme
                               start-line
                               start-column
                               start-offset
                               start-indent
                               named-char   ; literal value: 命名字符
                               start-leading-ws
                               #t   ; is-terminated?
                               #f))
                  ;; 不是有效的命名字符，回退到简单字符
                  (begin
                    (set-lexer-position! lexer saved-position)
                    (let ((lexeme (substring (lexer-source lexer)
                                             start-pos
                                             saved-position)))
                      (make-token 'CHARACTER
                                 lexeme
                                 start-line
                                 start-column
                                 start-offset
                                 start-indent
                                 (string-ref name 0)   ; 第一个字符
                                 start-leading-ws
                                 #t   ; is-terminated?
                                 #f))))))))))

    ;; 读取十六进制字符： #\xNN
    (define (read-hex-character lexer start-line start-column start-offset
                               start-indent start-leading-ws start-pos)
      (next-char! lexer)  ; 跳过 x
      ;; 收集所有可能是十六进制数字的字符
      (let loop ((hex-chars '())
                 (invalid-chars '()))
        (let ((ch (current-char lexer)))
          (cond
           ((and ch (hex-digit? ch))
            (next-char! lexer)
            (loop (cons ch hex-chars) invalid-chars))
           ((and ch (not (char-whitespace? ch)))
            ;; 任何非空白字符（包括符号、标点等）
            (next-char! lexer)
            (loop hex-chars (cons ch invalid-chars)))
           ((null? hex-chars)
            ;; 没有有效的十六进制数字
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
                         #t
                         #t)))
           (else
            ;; 检查是否有无效字符
            (if (not (null? invalid-chars))
                ;; 有无效字符，产生错误
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
                             #t
                             #t))
                ;; 没有无效字符，尝试解析十六进制数字
                (let* ((hex-str (list->string (reverse hex-chars)))
                       (char-code (hex-string->number hex-str)))
                  (if (and char-code (<= 0 char-code 255))
                      ;; 有效的 ASCII/扩展 ASCII 码点
                      (let ((lexeme (substring (lexer-source lexer)
                                               start-pos
                                               (lexer-position lexer))))
                        (make-token 'CHARACTER
                                   lexeme
                                   start-line
                                   start-column
                                   start-offset
                                   start-indent
                                   (integer->char char-code)   ; literal value
                                   start-leading-ws
                                   #t   ; is-terminated?
                                   #f))
                      ;; 无效的字符码点（超出 0-255 范围）
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
                                   #t
                                   #t))))))))))

    ;; ============================================
    ;; 辅助函数
    ;; ============================================

    ;; 检查是否是十六进制数字
    (define (hex-digit? ch)
      (or (char-numeric? ch)
          (char-ci=? ch #\a)
          (char-ci=? ch #\b)
          (char-ci=? ch #\c)
          (char-ci=? ch #\d)
          (char-ci=? ch #\e)
          (char-ci=? ch #\f)))

    ;; 十六进制字符串转数字
    (define (hex-string->number str)
      (let loop ((i 0)
                 (result 0))
        (if (= i (string-length str))
            result
            (let ((ch (string-ref str i)))
              (let ((digit (cond
                           ((char-numeric? ch) (- (char->integer ch) (char->integer #\0)))
                           ((char-ci=? ch #\a) 10)
                           ((char-ci=? ch #\b) 11)
                           ((char-ci=? ch #\c) 12)
                           ((char-ci=? ch #\d) 13)
                           ((char-ci=? ch #\e) 14)
                           ((char-ci=? ch #\f) 15)
                           (else #f))))
                (if digit
                    (loop (+ i 1) (+ (* result 16) digit))
                    #f))))))

    ;; 命名字符映射
    (define (named-character->char name)
      (let ((lower-name (string-downcase name)))
        (cond
         ((string=? lower-name "newline") #\newline)
         ((string=? lower-name "space") #\space)
         ((string=? lower-name "tab") #\tab)
         ((string=? lower-name "return") #\return)
         ((string=? lower-name "null") #\null)
         ((string=? lower-name "alarm") #\alarm)
         ((string=? lower-name "backspace") #\backspace)
         ((string=? lower-name "escape") #\escape)
         ((string=? lower-name "delete") #\delete)
         (else #f))))

  ) ; end of begin
) ; end of define-library