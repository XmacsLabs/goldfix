;;; lexer-base.scm - Goldfix 词法分析器基础模块

(define-library (goldfix lexer-base)
  (export make-token
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
          character-token?
          left-paren-token?
          right-paren-token?
          newline-token?
          whitespace-token?
          make-lexer
          lexer?
          lexer-source
          set-lexer-source!
          lexer-position
          set-lexer-position!
          lexer-line
          set-lexer-line!
          lexer-column
          set-lexer-column!
          lexer-offset
          set-lexer-offset!
          lexer-indent
          set-lexer-indent!
          lexer-leading-ws
          set-lexer-leading-ws!
          lexer-tab-width
          set-lexer-tab-width!
          current-char
          next-char!
          skip-whitespace!
          create-token)

  (import (scheme base)
          (scheme char))

  (begin
    ;; ============================================
    ;; Token 记录类型定义
    ;; ============================================

    (define-record-type <token>
      (make-token type lexeme line column offset indent-level literal-value
                  leading-whitespace is-terminated? has-error?)
      token?
      ;; 1. 核心属性
      (type          token-type)          ;; 符号：'NUMBER, 'EOF 等
      (lexeme        token-lexeme)        ;; 字符串：原始文本（必须保留原样）

      ;; 2. 物理位置参数
      (line          token-line)          ;; 整数：起始行号（1-based）
      (column        token-column)        ;; 整数：起始列号（1-based）
      (offset        token-offset)        ;; 整数：文件绝对位置（字符偏移量）
      (indent-level  token-indent)        ;; 整数：所在行的起始缩进量

      ;; 3. 关联数据
      (literal-value token-value)         ;; 任意：解析后的 Scheme 对象

      ;; 4. 格式化元数据
      (leading-whitespace token-leading-ws) ;; 字符串：该 Token 前面的空格

      ;; 5. 扫描状态标志
      (is-terminated? token-terminated?)  ;; 布尔值：是否完整
      (has-error?     token-has-error?))  ;; 布尔值：Token 内部是否有错误

    ;; Token 类型判断函数
    (define (eof-token? token)
      (eq? (token-type token) 'EOF))

    (define (number-token? token)
      (eq? (token-type token) 'NUMBER))

    (define (boolean-token? token)
      (eq? (token-type token) 'BOOLEAN))

    (define (character-token? token)
      (eq? (token-type token) 'CHARACTER))

    (define (left-paren-token? token)
      (eq? (token-type token) 'LEFT_PAREN))

    (define (right-paren-token? token)
      (eq? (token-type token) 'RIGHT_PAREN))

    (define (newline-token? token)
      (eq? (token-type token) 'NEWLINE))

    (define (whitespace-token? token)
      (eq? (token-type token) 'WHITESPACE))

    ;; ============================================
    ;; Lexer 记录类型定义
    ;; ============================================

    (define-record-type <lexer>
      (make-lexer-internal source position line column offset indent-level
                           leading-ws tab-width)
      lexer?
      (source lexer-source set-lexer-source!)        ;; 字符串：源代码
      (position lexer-position set-lexer-position!)  ;; 整数：当前位置
      (line lexer-line set-lexer-line!)              ;; 整数：当前行号
      (column lexer-column set-lexer-column!)        ;; 整数：当前列号
      (offset lexer-offset set-lexer-offset!)        ;; 整数：文件绝对位置
      (indent-level lexer-indent set-lexer-indent!)  ;; 整数：当前缩进级别
      (leading-ws lexer-leading-ws set-lexer-leading-ws!) ;; 字符串：累积的前导空格
      (tab-width lexer-tab-width set-lexer-tab-width!)) ;; 整数：Tab 宽度（默认 2）

    ;; 创建词法分析器
    (define (make-lexer source)
      (make-lexer-internal source 0 1 1 0 0 "" 2))

    ;; ============================================
    ;; 辅助函数
    ;; ============================================

    ;; 获取当前位置的字符
    (define (current-char lexer)
      (let ((source (lexer-source lexer))
            (pos (lexer-position lexer)))
        (if (< pos (string-length source))
            (string-ref source pos)
            #f)))

    ;; 前进到下一个字符
    (define (next-char! lexer)
      (let ((ch (current-char lexer)))
        (if ch
            (begin
              (set-lexer-position! lexer (+ (lexer-position lexer) 1))
              (set-lexer-offset! lexer (+ (lexer-offset lexer) 1))
              (if (char=? ch #\newline)
                  (begin
                    (set-lexer-line! lexer (+ (lexer-line lexer) 1))
                    (set-lexer-column! lexer 1)
                    (set-lexer-indent! lexer 0)
                    (set-lexer-leading-ws! lexer ""))
                  (set-lexer-column! lexer (+ (lexer-column lexer) 1))))
            #f))
      (current-char lexer))

    ;; 跳过空白字符并收集前导空格，遇到换行符时生成 NEWLINE token，遇到非空白字符时生成 WHITESPACE token
    (define (skip-whitespace! lexer)
      (let loop ((collecting-ws? #t))
        (let ((ch (current-char lexer)))
          (cond
           ((and ch (char-whitespace? ch))
            (if (char=? ch #\newline)
                ;; 遇到换行符，先检查是否有累积的前导空格需要生成 WHITESPACE token
                (let ((ws (lexer-leading-ws lexer)))
                  (if (> (string-length ws) 0)
                      ;; 有前导空格，先生成 WHITESPACE token
                      (let ((ws-lexeme ws)
                            (ws-start-line (lexer-line lexer))
                            (ws-start-column (- (lexer-column lexer) (string-length ws)))
                            (ws-start-offset (- (lexer-offset lexer) (string-length ws)))
                            (ws-start-indent (lexer-indent lexer))
                            (ws-start-leading-ws ""))
                        ;; 重置前导空格
                        (set-lexer-leading-ws! lexer "")
                        ;; 返回 WHITESPACE token，下次循环会处理换行符
                        (make-token 'WHITESPACE
                                   ws-lexeme
                                   ws-start-line
                                   ws-start-column
                                   ws-start-offset
                                   ws-start-indent
                                   #f
                                   ws-start-leading-ws
                                   #t
                                   #f))
                      ;; 没有前导空格，直接生成 NEWLINE token
                      (let ((lexeme (string ch))
                            (start-line (lexer-line lexer))
                            (start-column (lexer-column lexer))
                            (start-offset (lexer-offset lexer))
                            (start-indent (lexer-indent lexer))
                            (start-leading-ws (lexer-leading-ws lexer)))
                        (next-char! lexer)  ; 跳过换行符
                        ;; 重置前导空格，因为新的一行开始了
                        (set-lexer-leading-ws! lexer "")
                        ;; 返回 NEWLINE token
                        (make-token 'NEWLINE
                                   lexeme
                                   start-line
                                   start-column
                                   start-offset
                                   start-indent
                                   #f
                                   start-leading-ws
                                   #t
                                   #f))))
                ;; 普通空白字符，收集到前导空格中
                (begin
                  (if collecting-ws?
                      (set-lexer-leading-ws! lexer
                        (string-append (lexer-leading-ws lexer) (string ch)))
                      (void))
                  (next-char! lexer)
                  (loop collecting-ws?))))
           (else
            ;; 设置缩进：对于第一行，indent 总是0
            ;; 对于其他行，indent 是前导空格的长度（考虑 Tab 宽度）
            (if (= (lexer-line lexer) 1)
                (set-lexer-indent! lexer 0)
                (let ((ws (lexer-leading-ws lexer))
                      (tab-width (lexer-tab-width lexer)))
                  (let loop ((i 0) (total 0))
                    (if (= i (string-length ws))
                        (set-lexer-indent! lexer total)
                        (let ((ch (string-ref ws i)))
                          (if (char=? ch #\tab)
                              (loop (+ i 1) (+ total tab-width))
                              (loop (+ i 1) (+ total 1))))))))
            ;; 检查是否有累积的前导空格需要生成 WHITESPACE token
            (let ((ws (lexer-leading-ws lexer)))
              (if (and (> (string-length ws) 0)
                       ;; 只有当空格后面不是换行符时才生成 WHITESPACE token
                       ;; 因为换行符已经生成了 NEWLINE token
                       (let ((next-ch ch))
                         (not (and next-ch (char=? next-ch #\newline)))))
                  (let ((lexeme ws)
                        (start-line (lexer-line lexer))
                        (start-column (- (lexer-column lexer) (string-length ws)))
                        (start-offset (- (lexer-offset lexer) (string-length ws)))
                        (start-indent (lexer-indent lexer))
                        (start-leading-ws ""))
                    ;; 重置前导空格，因为已经生成了 WHITESPACE token
                    (set-lexer-leading-ws! lexer "")
                    ;; 返回 WHITESPACE token
                    (make-token 'WHITESPACE
                               lexeme
                               start-line
                               start-column
                               start-offset
                               start-indent
                               #f
                               start-leading-ws
                               #t
                               #f))
                  ;; 返回 #f 表示没有生成 token
                  #f)))))))

    ;; 创建 Token 的辅助函数（用于 EOF 等特殊情况）
    (define (create-token lexer type lexeme literal-value)
      (make-token type
                  lexeme
                  (lexer-line lexer)
                  (lexer-column lexer)
                  (lexer-offset lexer)
                  (lexer-indent lexer)
                  literal-value
                  (lexer-leading-ws lexer)
                  #t   ; is-terminated?
                  #f)) ; has-error?

  ) ; end of begin
) ; end of define-library