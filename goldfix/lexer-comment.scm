;;; lexer-comment.scm - Goldfix 注释词法分析器模块

(define-library (goldfix lexer-comment)
  (export read-comment
          comment-token?)

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
                ;; 返回 COMMENT_LINE token
                (let ((token (make-token 'COMMENT_LINE
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

    ;; 判断是否为注释 token
    (define (comment-token? token)
      (eq? (token-type token) 'COMMENT_LINE))

  ) ; end of begin
) ; end of define-library