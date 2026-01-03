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

    ;; ============================================
    ;; Lexer 记录类型定义
    ;; ============================================

    (define-record-type <lexer>
      (make-lexer-internal source position line column offset indent-level
                           leading-ws)
      lexer?
      (source lexer-source set-lexer-source!)        ;; 字符串：源代码
      (position lexer-position set-lexer-position!)  ;; 整数：当前位置
      (line lexer-line set-lexer-line!)              ;; 整数：当前行号
      (column lexer-column set-lexer-column!)        ;; 整数：当前列号
      (offset lexer-offset set-lexer-offset!)        ;; 整数：文件绝对位置
      (indent-level lexer-indent set-lexer-indent!)  ;; 整数：当前缩进级别
      (leading-ws lexer-leading-ws set-lexer-leading-ws!)) ;; 字符串：累积的前导空格

    ;; 创建词法分析器
    (define (make-lexer source)
      (make-lexer-internal source 0 1 1 0 0 ""))

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

    ;; 跳过空白字符并收集前导空格
    (define (skip-whitespace! lexer)
      (let loop ()
        (let ((ch (current-char lexer)))
          (cond
           ((and ch (char-whitespace? ch))
            (set-lexer-leading-ws! lexer
              (string-append (lexer-leading-ws lexer) (string ch)))
            (next-char! lexer)
            (loop))
           (else
            ;; 设置缩进：对于第一行，indent 总是0
            ;; 对于其他行，indent 是前导空格的长度
            (if (= (lexer-line lexer) 1)
                (set-lexer-indent! lexer 0)
                (let ((ws-length (string-length (lexer-leading-ws lexer))))
                  (set-lexer-indent! lexer ws-length))))))))

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