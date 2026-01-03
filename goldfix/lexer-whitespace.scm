;;; lexer-whitespace.scm - Goldfix 空白字符词法分析器模块

(define-library (goldfix lexer-whitespace)
  (export read-whitespace)

  (import (scheme base)
          (scheme char)
          (goldfix lexer-base))

  (begin
    ;; ============================================
    ;; 空白字符读取函数
    ;; ============================================

    ;; 读取空白字符（空格、制表符等，但不包括换行符）
    (define (read-whitespace lexer)
      (let ((ch (current-char lexer)))
        (if (and ch (char-whitespace? ch) (not (char=? ch #\newline)))
            ;; 收集连续空白字符
            (let ((start-pos (lexer-position lexer))
                  (start-line (lexer-line lexer))
                  (start-column (lexer-column lexer))
                  (start-offset (lexer-offset lexer))
                  (start-indent (lexer-indent lexer))
                  (start-leading-ws (lexer-leading-ws lexer)))
              ;; 跳过第一个空白字符
              (next-char! lexer)
              ;; 继续收集后续空白字符
              (let loop ()
                (let ((next-ch (current-char lexer)))
                  (if (and next-ch (char-whitespace? next-ch) (not (char=? next-ch #\newline)))
                      (begin
                        (next-char! lexer)
                        (loop))
                      #f)))
              (let ((lexeme (substring (lexer-source lexer)
                                       start-pos
                                       (lexer-position lexer))))
                ;; 返回 WHITESPACE token
                (let ((token (make-token 'WHITESPACE
                                        lexeme
                                        start-line
                                        start-column
                                        start-offset
                                        start-indent
                                        #f
                                        start-leading-ws
                                        #t
                                        #f)))
                  ;; 重置前导空格，因为已经生成了 WHITESPACE token
                  (set-lexer-leading-ws! lexer "")
                  token)))
            #f)))  ; 不是空白字符，返回 #f

  ) ; end of begin
) ; end of define-library