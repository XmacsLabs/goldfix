;;; lexer-identifier.scm - Goldfix 标识符词法分析模块

(define-library (goldfix lexer-identifier)
  (export read-identifier
          identifier-token?
          initial-char?
          subsequent-char?
          peculiar-identifier?)

  (import (scheme base)
          (scheme char)
          (goldfix lexer-base))

  (begin
    ;; ============================================
    ;; 字符分类函数
    ;; ============================================

    ;; 检查是否为 Initial 字符（起始字符）
    (define (initial-char? ch)
      (or (char-alphabetic? ch)           ; Unicode 字母
          (not (not (memv ch '(#\! #\$ #\% #\&    ; 特殊符号
                               #\* #\/ #\: #\<
                               #\= #\> #\? #\^
                               #\_ #\~))))))

    ;; 检查是否为 Subsequent 字符（后续字符）
    (define (subsequent-char? ch)
      (or (initial-char? ch)
          (char-numeric? ch)              ; Unicode 数字
          (not (not (memv ch '(#\+ #\- #\. #\@)))))) ; 显式符号

    ;; 检查是否为特殊标识符（Peculiar Identifier）
    (define (peculiar-identifier? str)
      (or (string=? str "+")
          (string=? str "-")
          (string=? str "...")
          (string=? str "->")
          (and (> (string-length str) 2)
               (string=? (substring str 0 2) "->")
               (let loop ((i 2))
                 (if (= i (string-length str))
                     #t
                     (and (subsequent-char? (string-ref str i))
                          (loop (+ i 1))))))
          (and (> (string-length str) 1)
               (let ((first-char (string-ref str 0)))
                 (or (char=? first-char #\+)
                     (char=? first-char #\-)))
               (let loop ((i 1))
                 (if (= i (string-length str))
                     #t
                     (and (subsequent-char? (string-ref str i))
                          (loop (+ i 1))))))))

    ;; ============================================
    ;; 标识符读取函数
    ;; ============================================

    ;; 读取标识符的辅助函数（处理普通标识符）
    (define (read-normal-identifier lexer start-pos)
      (let loop ()
        (let ((ch (current-char lexer)))
          (if (and ch (subsequent-char? ch))
              (begin
                (next-char! lexer)
                (loop))
              ;; 读取完成，创建 token
              (let ((lexeme (substring (lexer-source lexer)
                                       start-pos
                                       (lexer-position lexer)))
                    (start-line (lexer-line lexer))
                    (start-column (lexer-column lexer))
                    (start-offset (lexer-offset lexer))
                    (start-indent (lexer-indent lexer))
                    (start-leading-ws (lexer-leading-ws lexer)))
                (make-token 'IDENTIFIER
                           lexeme
                           start-line
                           start-column
                           start-offset
                           start-indent
                           #f  ; literal-value
                           start-leading-ws
                           #t  ; is-terminated?
                           #f)))))) ; has-error?

    ;; 主标识符读取函数
    (define (read-identifier lexer)
      (let ((start-pos (lexer-position lexer))
            (start-line (lexer-line lexer))
            (start-column (lexer-column lexer))
            (start-offset (lexer-offset lexer))
            (start-indent (lexer-indent lexer))
            (start-leading-ws (lexer-leading-ws lexer))
            (first-char (current-char lexer)))

        (cond
         ;; 处理点号开头的特殊情况
         ((char=? first-char #\.)
          (next-char! lexer)
          (let ((next-ch (current-char lexer)))
            (cond
             ;; 单独的 . 是语法符号，不是标识符
             ((or (not next-ch)
                  (char-whitespace? next-ch)
                  (char=? next-ch #\))
                  (char=? next-ch #\())
              ;; 回退，让主 lexer 处理
              (set-lexer-position! lexer start-pos)
              (set-lexer-column! lexer start-column)
              (set-lexer-offset! lexer start-offset)
              #f)
             ;; . 后面是数字，可能是小数
             ((char-numeric? next-ch)
              ;; 回退，让数字解析器处理
              (set-lexer-position! lexer start-pos)
              (set-lexer-column! lexer start-column)
              (set-lexer-offset! lexer start-offset)
              #f)
             ;; . 后面是合法后续字符，如 .foo
             ((subsequent-char? next-ch)
              (read-normal-identifier lexer start-pos))
             (else
              ;; 无效的 . 组合
              (set-lexer-position! lexer start-pos)
              (set-lexer-column! lexer start-column)
              (set-lexer-offset! lexer start-offset)
              #f))))

         ;; 处理 + 或 - 开头的特殊情况
         ((or (char=? first-char #\+) (char=? first-char #\-))
          (next-char! lexer)
          (let ((next-ch (current-char lexer)))
            (cond
             ;; 单独的 + 或 -
             ((or (not next-ch)
                  (char-whitespace? next-ch)
                  (char=? next-ch #\))
                  (char=? next-ch #\())
              (let ((lexeme (string first-char)))
                (make-token 'IDENTIFIER
                           lexeme
                           start-line
                           start-column
                           start-offset
                           start-indent
                           #f
                           start-leading-ws
                           #t
                           #f)))
             ;; + 或 - 后面是合法字符，继续读取
             ((subsequent-char? next-ch)
              (read-normal-identifier lexer start-pos))
             (else
              ;; 无效的组合
              (let ((lexeme (string first-char)))
                (make-token 'IDENTIFIER
                           lexeme
                           start-line
                           start-column
                           start-offset
                           start-indent
                           #f
                           start-leading-ws
                           #t
                           #f))))))

         ;; 普通标识符
         ((initial-char? first-char)
          (next-char! lexer)
          (read-normal-identifier lexer start-pos))

         ;; 不是标识符
         (else #f))))

    ;; ============================================
    ;; 辅助函数
    ;; ============================================

    ;; 检查是否为标识符 token
    (define (identifier-token? token)
      (eq? (token-type token) 'IDENTIFIER))

  ) ; end of begin
) ; end of define-library