;;; lexer.scm - Goldfix 词法分析器模块（数字场景）

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
          number-token?)

  (import (scheme base)
          (scheme char))

  (begin
    ;; ============================================
    ;; Token 记录类型定义（专注于数字场景）
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
      (is-terminated? token-terminated?)  ;; 布尔值：是否完整（对数字总是 #t）
      (has-error?     token-has-error?))  ;; 布尔值：Token 内部是否有错误

    ;; Token 类型判断函数
    (define (eof-token? token)
      (eq? (token-type token) 'EOF))

    (define (number-token? token)
      (eq? (token-type token) 'NUMBER))

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
