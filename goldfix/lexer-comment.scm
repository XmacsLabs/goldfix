;;; lexer-comment.scm - Goldfix 注释词法分析器模块

(define-library (goldfix lexer-comment)
  (export read-comment
          read-block-comment
          inline-comment-token?
          block-comment-token?
          unterminated-block-comment-token?)

  (import (scheme base)
          (scheme char)
          (goldfix lexer-base))

  (begin
    ;; ============================================
    ;; 注释读取函数
    ;; ============================================

    ;; 读取行注释（以分号开始，直到行尾）
    (define (read-comment lexer)
      (let ((ch (current-char lexer)))
        (if (and ch (char=? ch #\;))
            ;; 收集注释内容直到行尾
            (let ((start-pos (lexer-position lexer))
                  (start-line (lexer-line lexer))
                  (start-column (lexer-column lexer))
                  (start-offset (lexer-offset lexer))
                  (start-indent (lexer-indent lexer))
                  (start-leading-ws (lexer-leading-ws lexer)))
              ;; 跳过分号
              (next-char! lexer)
              ;; 收集注释内容直到行尾或文件结束
              (let loop ()
                (let ((next-ch (current-char lexer)))
                  (if (and next-ch (not (char=? next-ch #\newline)))
                      (begin
                        (next-char! lexer)
                        (loop))
                      #f)))
              (let ((lexeme (substring (lexer-source lexer)
                                       start-pos
                                       (lexer-position lexer))))
                ;; 返回 INLINE_COMMENT token
                (let ((token (make-token 'INLINE_COMMENT
                                        lexeme
                                        start-line
                                        start-column
                                        start-offset
                                        start-indent
                                        #f
                                        start-leading-ws
                                        #t
                                        #f)))
                  ;; 重置前导空格，因为注释已经生成了 token
                  (set-lexer-leading-ws! lexer "")
                  token)))
            #f)))  ; 不是注释，返回 #f

    ;; ============================================
    ;; 注释 token 判断函数
    ;; ============================================

    ;; 判断是否为行内注释 token
    (define (inline-comment-token? token)
      (eq? (token-type token) 'INLINE_COMMENT))

    ;; ============================================
    ;; 块注释读取函数
    ;; ============================================

    ;; 读取块注释（以 #| 开始，以 |# 结束，支持嵌套）
    (define (read-block-comment lexer)
      (let ((ch1 (current-char lexer))
            (next-pos (+ (lexer-position lexer) 1)))
        (if (and ch1 (char=? ch1 #\#)
                 (< next-pos (string-length (lexer-source lexer)))
                 (char=? (string-ref (lexer-source lexer) next-pos) #\|))
            ;; 开始读取块注释
            (let ((start-pos (lexer-position lexer))
                  (start-line (lexer-line lexer))
                  (start-column (lexer-column lexer))
                  (start-offset (lexer-offset lexer))
                  (start-indent (lexer-indent lexer))
                  (start-leading-ws (lexer-leading-ws lexer))
                  (nesting-level 1))
              ;; 跳过 #|
              (next-char! lexer)  ; 跳过 #
              (next-char! lexer)  ; 跳过 |

              ;; 收集注释内容直到找到匹配的 |# 或文件结束
              (let ((final-level
                     (let loop ((level nesting-level))
                       (if (= level 0)
                           0  ; 块注释结束
                           (let ((ch (current-char lexer)))
                             (cond
                              ((not ch)
                               ;; 文件结束，块注释未闭合，返回当前嵌套级别
                               level)
                              ((and (char=? ch #\|)
                                    (< (+ (lexer-position lexer) 1) (string-length (lexer-source lexer)))
                                    (char=? (string-ref (lexer-source lexer) (+ (lexer-position lexer) 1)) #\#))
                               ;; 找到 |#，减少嵌套级别
                               (next-char! lexer)  ; 跳过 |
                               (next-char! lexer)  ; 跳过 #
                               (loop (- level 1)))
                              ((and (char=? ch #\#)
                                    (< (+ (lexer-position lexer) 1) (string-length (lexer-source lexer)))
                                    (char=? (string-ref (lexer-source lexer) (+ (lexer-position lexer) 1)) #\|))
                               ;; 找到 #|，增加嵌套级别
                               (next-char! lexer)  ; 跳过 #
                               (next-char! lexer)  ; 跳过 |
                               (loop (+ level 1)))
                              (else
                               ;; 普通字符，继续读取
                               (next-char! lexer)
                               (loop level))))))))
                (let ((lexeme (substring (lexer-source lexer)
                                         start-pos
                                         (lexer-position lexer))))
                  (if (= final-level 0)
                      ;; 块注释正常闭合
                      (let ((token (make-token 'BLOCK_COMMENT
                                              lexeme
                                              start-line
                                              start-column
                                              start-offset
                                              start-indent
                                              #f
                                              start-leading-ws
                                              #t
                                              #f)))
                        ;; 重置前导空格
                        (set-lexer-leading-ws! lexer "")
                        token)
                      ;; 块注释未闭合
                      (make-token 'UNTERMINATED_BLOCK_COMMENT
                                 lexeme
                                 start-line
                                 start-column
                                 start-offset
                                 start-indent
                                 #f
                                 start-leading-ws
                                 #f
                                 #t)))))
            #f)))  ; 不是块注释，返回 #f

    ;; ============================================
    ;; 块注释 token 判断函数
    ;; ============================================

    ;; 判断是否为块注释 token
    (define (block-comment-token? token)
      (eq? (token-type token) 'BLOCK_COMMENT))

    ;; 判断是否为未闭合的块注释 token
    (define (unterminated-block-comment-token? token)
      (eq? (token-type token) 'UNTERMINATED_BLOCK_COMMENT))

  ) ; end of begin
) ; end of define-library