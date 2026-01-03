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
          number-token?
          boolean-token?
          identifier-token?
          character-token?
          left-paren-token?
          right-paren-token?
          newline-token?)

  (import (scheme base)
          (scheme char)
          (goldfix lexer-base)
          (goldfix lexer-number)
          (goldfix lexer-boolean)
          (goldfix lexer-identifier)
          (goldfix lexer-character))

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
    (define boolean-token? boolean-token?)

    ;; 重新导出标识符模块的函数
    (define identifier-token? identifier-token?)

    ;; 重新导出字符模块的函数
    (define character-token? character-token?)

    ;; 重新导出括号 token 函数
    (define left-paren-token? left-paren-token?)
    (define right-paren-token? right-paren-token?)

    ;; 重新导出 NEWLINE token 函数
    (define newline-token? newline-token?)

    ;; Lexer 相关函数
    (define make-lexer make-lexer)

    ;; ============================================
    ;; 主扫描函数
    ;; ============================================

    (define (lexer-next-token lexer)
      (let ((newline-token (skip-whitespace! lexer)))
        (if newline-token
            ;; skip-whitespace! 返回了一个 NEWLINE token
            newline-token
            ;; 继续处理其他 token
            (let ((ch (current-char lexer)))
              (cond
               ((not ch)
                (create-token lexer 'EOF "" #f))
               ((char=? ch #\#)
                ;; 检查是布尔值、数字还是字符
                (let ((next-pos (+ (lexer-position lexer) 1)))
                  (if (< next-pos (string-length (lexer-source lexer)))
                      (let ((next-ch (string-ref (lexer-source lexer) next-pos)))
                        (cond
                         ;; 布尔值：后面是 t/f（小写）
                         ((or (char=? next-ch #\t) (char=? next-ch #\f))
                          (read-boolean lexer))
                         ;; 数字：后面是 b/B/o/O/d/D/x/X
                         ((or (char=? next-ch #\b) (char=? next-ch #\B)
                              (char=? next-ch #\o) (char=? next-ch #\O)
                              (char=? next-ch #\d) (char=? next-ch #\D)
                              (char=? next-ch #\x) (char=? next-ch #\X))
                          (read-number lexer))
                         ;; 字符：后面是 \
                         ((char=? next-ch #\\)
                          (read-character lexer))
                         (else
                          ;; 无效的 # 前缀
                          (let ((start-line (lexer-line lexer))
                                (start-column (lexer-column lexer))
                                (start-offset (lexer-offset lexer))
                                (start-indent (lexer-indent lexer))
                                (start-leading-ws (lexer-leading-ws lexer))
                                (start-pos (lexer-position lexer)))
                            (next-char! lexer)  ; 跳过 #
                            (next-char! lexer)  ; 跳过无效字符
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
                                         #t))))))
                      ;; 只有 # 字符
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
                                   #t)))))
               ((char-numeric? ch)
                (read-number lexer))
               ((char=? ch #\()
                ;; 左括号
                (let ((lexeme (string ch))
                      (start-line (lexer-line lexer))
                      (start-column (lexer-column lexer))
                      (start-offset (lexer-offset lexer))
                      (start-indent (lexer-indent lexer))
                      (start-leading-ws (lexer-leading-ws lexer)))
                  (next-char! lexer)
                  (make-token 'LEFT_PAREN
                             lexeme
                             start-line
                             start-column
                             start-offset
                             start-indent
                             #f
                             start-leading-ws
                             #t
                             #f)))
               ((char=? ch #\))
                ;; 右括号
                (let ((lexeme (string ch))
                      (start-line (lexer-line lexer))
                      (start-column (lexer-column lexer))
                      (start-offset (lexer-offset lexer))
                      (start-indent (lexer-indent lexer))
                      (start-leading-ws (lexer-leading-ws lexer)))
                  (next-char! lexer)
                  (make-token 'RIGHT_PAREN
                             lexeme
                             start-line
                             start-column
                             start-offset
                             start-indent
                             #f
                             start-leading-ws
                             #t
                             #f)))
               (else
                (let ((identifier-token (read-identifier lexer)))
                  (if identifier-token
                      ;; 数字优先规则：检查标识符是否实际上是数字
                      (let ((lexeme (token-lexeme identifier-token)))
                        ;; 简单检查：如果 lexeme 全部由数字组成，尝试解析为数字
                        (if (and (> (string-length lexeme) 0)
                                 (let loop ((i 0))
                                   (if (= i (string-length lexeme))
                                       #t
                                       (and (char-numeric? (string-ref lexeme i))
                                            (loop (+ i 1))))))
                            (let ((num (string->number lexeme)))
                              (if num
                                  ;; 创建 NUMBER token 替换原来的 IDENTIFIER token
                                  (make-token 'NUMBER
                                              lexeme
                                              (token-line identifier-token)
                                              (token-column identifier-token)
                                              (token-offset identifier-token)
                                              (token-indent identifier-token)
                                              num
                                              (token-leading-ws identifier-token)
                                              #t
                                              #f)
                                  ;; 解析失败，返回原始标识符 token
                                  identifier-token))
                            ;; 不是全数字，返回原始标识符 token
                            identifier-token))
                      ;; 不是标识符，处理为错误
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
                                   #t))))))))))

  ) ; end of begin
) ; end of define-library