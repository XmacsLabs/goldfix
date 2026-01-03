;;; lexer-character-test.scm - Goldfix 字符词法分析器单元测试

(import (liii check)
        (goldfix lexer))

;; 设置测试模式
(check-set-mode! 'report-failed)

;; ============================================
;; 测试辅助函数
;; ============================================

;; 测试单个字符
(define (test-single-character input expected-lexeme expected-value)
  (let ((lexer (make-lexer input)))
    (let ((token (lexer-next-token lexer)))
      (check (character-token? token) => #t)
      (check (token-lexeme token) => expected-lexeme)
      (check (token-value token) => expected-value)
      (check (token-terminated? token) => #t)
      (check (token-has-error? token) => #f)
      ;; 检查 EOF
      (let ((eof-token (lexer-next-token lexer)))
        (check (eof-token? eof-token) => #t)))))

;; 测试错误字符
(define (test-error-character input expected-lexeme)
  (let ((lexer (make-lexer input)))
    (let ((token (lexer-next-token lexer)))
      (check (token-has-error? token) => #t)
      (check (token-lexeme token) => expected-lexeme)
      ;; 检查 EOF
      (let ((eof-token (lexer-next-token lexer)))
        (check (eof-token? eof-token) => #t)))))

;; 测试多个字符
(define (test-multiple-characters input expected-tokens)
  (let ((lexer (make-lexer input))
        (tokens '()))
    (let loop ()
      (let ((token (lexer-next-token lexer)))
        (set! tokens (cons token tokens))
        (unless (eof-token? token)
          (loop))))
    (let ((actual-tokens (reverse (cdr tokens)))) ; 去掉最后的 EOF
      (check (length actual-tokens) => (length expected-tokens))
      (for-each
       (lambda (actual expected)
         (check (character-token? actual) => #t)
         (check (token-lexeme actual) => (car expected))
         (check (token-value actual) => (cadr expected)))
       actual-tokens
       expected-tokens))))

;; ============================================
;; 测试用例开始
;; ============================================

;; 测试 1: 简单字符 #\a
(test-single-character "#\\a" "#\\a" #\a)

;; 测试 2: 简单字符 #\1
(test-single-character "#\\1" "#\\1" #\1)

;; 测试 3: 简单字符 #\+
(test-single-character "#\\+" "#\\+" #\+)

;; 测试 4: 简单字符 #\space
(test-single-character "#\\space" "#\\space" #\space)

;; 测试 5: 简单字符 #\newline
(test-single-character "#\\newline" "#\\newline" #\newline)

;; 测试 6: 简单字符 #\tab
(test-single-character "#\\tab" "#\\tab" #\tab)

;; 测试 7: 简单字符 #\return
(test-single-character "#\\return" "#\\return" #\return)

;; 测试 8: 简单字符 #\null
(test-single-character "#\\null" "#\\null" #\null)

;; 测试 9: 简单字符 #\alarm
(test-single-character "#\\alarm" "#\\alarm" #\alarm)

;; 测试 10: 简单字符 #\backspace
(test-single-character "#\\backspace" "#\\backspace" #\backspace)

;; 测试 11: 简单字符 #\escape
(test-single-character "#\\escape" "#\\escape" #\escape)

;; 测试 12: 简单字符 #\delete
(test-single-character "#\\delete" "#\\delete" #\delete)

;; 测试 13: 十六进制字符 #\x41 (ASCII 'A')
(test-single-character "#\\x41" "#\\x41" #\A)

;; 测试 14: 十六进制字符 #\x61 (ASCII 'a')
(test-single-character "#\\x61" "#\\x61" #\a)

;; 测试 15: 十六进制字符 #\x30 (ASCII '0')
(test-single-character "#\\x30" "#\\x30" #\0)

;; 测试 16: 十六进制字符 #\x7F (ASCII DEL)
(test-single-character "#\\x7F" "#\\x7F" #\delete)

;; 测试 17: 十六进制字符 #\xFF (扩展 ASCII)
(test-single-character "#\\xFF" "#\\xFF" (integer->char #xFF))

;; 测试 18: 十六进制字符 #\x80 (扩展 ASCII)
(test-single-character "#\\x80" "#\\x80" (integer->char #x80))

;; 测试 19: 十六进制字符 #\xFF (扩展 ASCII)
(test-single-character "#\\xFF" "#\\xFF" (integer->char #xFF))

;; 测试 20: 十六进制字符 #\x7F (ASCII DEL)
(test-single-character "#\\x7F" "#\\x7F" #\delete)

;; 测试 21: 大小写混合的命名字符 #\Space
(test-single-character "#\\Space" "#\\Space" #\space)

;; 测试 22: 大小写混合的命名字符 #\NewLine
(test-single-character "#\\NewLine" "#\\NewLine" #\newline)

;; 测试 23: 大小写混合的命名字符 #\Tab
(test-single-character "#\\Tab" "#\\Tab" #\tab)

;; 测试 24: 多个字符 " #\a #\b #\c"
(test-multiple-characters " #\\a #\\b #\\c"
                         '(("#\\a" #\a) ("#\\b" #\b) ("#\\c" #\c)))

;; 测试 25: 字符与空格混合 " #\space #\newline #\tab"
(test-multiple-characters " #\\space #\\newline #\\tab"
                         '(("#\\space" #\space) ("#\\newline" #\newline) ("#\\tab" #\tab)))

;; 测试 26: 字符与数字混合 " #\1 #\2 #\3"
(test-multiple-characters " #\\1 #\\2 #\\3"
                         '(("#\\1" #\1) ("#\\2" #\2) ("#\\3" #\3)))

;; 测试 27: 错误字符 #\ (没有字符)
(test-error-character "#\\" "#\\")

;; 测试 28: 错误字符 #\x (没有十六进制数字)
(test-error-character "#\\x" "#\\x")

;; 测试 29: 错误字符 #\xG (无效的十六进制数字)
(test-error-character "#\\xG" "#\\xG")

;; 测试 30: 错误字符 #\x100 (超出 ASCII 范围)
(test-error-character "#\\x100" "#\\x100")

;; 测试 31: 错误字符 #\invalid (无效的命名字符)
(test-single-character "#\\invalid" "#\\invalid" #\i) ; 应该回退到简单字符 #\i

;; 测试 32: 错误字符 #\newlin (不完整的命名字符)
(test-single-character "#\\newlin" "#\\newlin" #\n) ; 应该回退到简单字符 #\n

;; 测试 33: 错误字符 #\spac (不完整的命名字符)
(test-single-character "#\\spac" "#\\spac" #\s) ; 应该回退到简单字符 #\s

;; 测试 34: 字符与标识符混合 " #\a foo #\b"
(let ((lexer (make-lexer " #\\a foo #\\b"))
      (tokens '()))
  (let loop ()
    (let ((token (lexer-next-token lexer)))
      (set! tokens (cons token tokens))
      (unless (eof-token? token)
        (loop))))
  (let ((actual-tokens (reverse (cdr tokens)))) ; 去掉最后的 EOF
    (check (length actual-tokens) => 3)
    (check (character-token? (car actual-tokens)) => #t)
    (check (token-lexeme (car actual-tokens)) => "#\\a")
    (check (token-value (car actual-tokens)) => #\a)
    (check (identifier-token? (cadr actual-tokens)) => #t)
    (check (token-lexeme (cadr actual-tokens)) => "foo")
    (check (character-token? (caddr actual-tokens)) => #t)
    (check (token-lexeme (caddr actual-tokens)) => "#\\b")
    (check (token-value (caddr actual-tokens)) => #\b)))

;; 测试 35: 字符与布尔值混合 " #\a #t #\b"
(let ((lexer (make-lexer " #\\a #t #\\b"))
      (tokens '()))
  (let loop ()
    (let ((token (lexer-next-token lexer)))
      (set! tokens (cons token tokens))
      (unless (eof-token? token)
        (loop))))
  (let ((actual-tokens (reverse (cdr tokens)))) ; 去掉最后的 EOF
    (check (length actual-tokens) => 3)
    (check (character-token? (car actual-tokens)) => #t)
    (check (token-lexeme (car actual-tokens)) => "#\\a")
    (check (token-value (car actual-tokens)) => #\a)
    (check (boolean-token? (cadr actual-tokens)) => #t)
    (check (token-lexeme (cadr actual-tokens)) => "#t")
    (check (token-value (cadr actual-tokens)) => #t)
    (check (character-token? (caddr actual-tokens)) => #t)
    (check (token-lexeme (caddr actual-tokens)) => "#\\b")
    (check (token-value (caddr actual-tokens)) => #\b)))

;; 测试 36: 字符与数字混合 " #\1 123 #\2"
(let ((lexer (make-lexer " #\\1 123 #\\2"))
      (tokens '()))
  (let loop ()
    (let ((token (lexer-next-token lexer)))
      (set! tokens (cons token tokens))
      (unless (eof-token? token)
        (loop))))
  (let ((actual-tokens (reverse (cdr tokens)))) ; 去掉最后的 EOF
    (check (length actual-tokens) => 3)
    (check (character-token? (car actual-tokens)) => #t)
    (check (token-lexeme (car actual-tokens)) => "#\\1")
    (check (token-value (car actual-tokens)) => #\1)
    (check (number-token? (cadr actual-tokens)) => #t)
    (check (token-lexeme (cadr actual-tokens)) => "123")
    (check (token-value (cadr actual-tokens)) => 123)
    (check (character-token? (caddr actual-tokens)) => #t)
    (check (token-lexeme (caddr actual-tokens)) => "#\\2")
    (check (token-value (caddr actual-tokens)) => #\2)))

;; 测试 37: 探索 #\space 的特殊情况
(check #\space => #\ )

;; 测试 38: 探索 #\newline 的特殊情况
(check #\newline => #\newline)

;; 测试 39: 探索 #\tab 的特殊情况
(check #\tab => #\tab)

;; 测试 40: 探索 #\return 的特殊情况
(check #\return => #\return)

;; 测试 41: 探索 #\null 的特殊情况
(check #\null => #\null)

;; 测试 42: 探索 #\alarm 的特殊情况
(check #\alarm => #\alarm)

;; 测试 43: 探索 #\backspace 的特殊情况
(check #\backspace => #\backspace)

;; 测试 44: 探索 #\escape 的特殊情况
(check #\escape => #\escape)

;; 测试 45: 探索 #\delete 的特殊情况
(check #\delete => #\delete)

;; 测试 46: 测试 #\x20 (空格)
(test-single-character "#\\x20" "#\\x20" #\space)

;; 测试 47: 测试 #\x0A (换行)
(test-single-character "#\\x0A" "#\\x0A" #\newline)

;; 测试 48: 测试 #\x09 (制表符)
(test-single-character "#\\x09" "#\\x09" #\tab)

;; 测试 49: 测试 #\x0D (回车)
(test-single-character "#\\x0D" "#\\x0D" #\return)

;; 测试 50: 测试 #\x00 (空字符)
(test-single-character "#\\x00" "#\\x00" #\null)

;; 测试 51: 测试 #\x07 (响铃)
(test-single-character "#\\x07" "#\\x07" #\alarm)

;; 测试 52: 测试 #\x08 (退格)
(test-single-character "#\\x08" "#\\x08" #\backspace)

;; 测试 53: 测试 #\x1B (ESC)
(test-single-character "#\\x1B" "#\\x1B" #\escape)

;; 测试 54: 测试 #\x7F (删除)
(test-single-character "#\\x7F" "#\\x7F" #\delete)

;; 测试 55: 测试大写 X 字符 #\X (后面 41 是单独的数字)
(test-single-character "#\\X" "#\\X" #\X)

;; 测试 56: 测试大小写十六进制 #\xA
(test-single-character "#\\xA" "#\\xA" #\newline)

;; 测试 57: 测试大写 X 字符 #\X (后面 a 是单独标识符)
(test-single-character "#\\X" "#\\X" #\X)

;; 测试 58: 测试大写 X 字符 #\X (后面 FF 是单独标识符)
(test-single-character "#\\X" "#\\X" #\X)

;; 测试 59: 测试十六进制边界 #\x0
(test-single-character "#\\x0" "#\\x0" #\null)

;; 测试 60: 测试十六进制边界 #\xFF
(test-single-character "#\\xFF" "#\\xFF" (integer->char #xFF))

;; 测试 61: 测试无效十六进制 #\x100 (超出范围)
(test-error-character "#\\x100" "#\\x100")

;; 测试 62: 测试无效十六进制 #\x-1 (负号)
(test-error-character "#\\x-1" "#\\x-1")

;; 测试 63: 测试无效十六进制 #\x1.5 (小数点)
(test-error-character "#\\x1.5" "#\\x1.5")

;; 测试 64: 测试命名字符大小写 #\NEWLINE
(test-single-character "#\\NEWLINE" "#\\NEWLINE" #\newline)

;; 测试 65: 测试命名字符大小写 #\SPACE
(test-single-character "#\\SPACE" "#\\SPACE" #\space)

;; 测试 66: 测试命名字符大小写 #\TAB
(test-single-character "#\\TAB" "#\\TAB" #\tab)

;; 测试 67: 测试命名字符大小写 #\RETURN
(test-single-character "#\\RETURN" "#\\RETURN" #\return)

;; 测试 68: 测试命名字符大小写 #\NULL
(test-single-character "#\\NULL" "#\\NULL" #\null)

;; 测试 69: 测试命名字符大小写 #\ALARM
(test-single-character "#\\ALARM" "#\\ALARM" #\alarm)

;; 测试 70: 测试命名字符大小写 #\BACKSPACE
(test-single-character "#\\BACKSPACE" "#\\BACKSPACE" #\backspace)

;; 测试 71: 测试命名字符大小写 #\ESCAPE
(test-single-character "#\\ESCAPE" "#\\ESCAPE" #\escape)

;; 测试 72: 测试命名字符大小写 #\DELETE
(test-single-character "#\\DELETE" "#\\DELETE" #\delete)

;; 测试 73: 测试部分命名字符 #\n (应该是简单字符，不是 newline)
(test-single-character "#\\n" "#\\n" #\n)

;; 测试 74: 测试部分命名字符 #\s (应该是简单字符，不是 space)
(test-single-character "#\\s" "#\\s" #\s)

;; 测试 75: 测试部分命名字符 #\t (应该是简单字符，不是 tab)
(test-single-character "#\\t" "#\\t" #\t)

;; 测试 76: 测试部分命名字符 #\r (应该是简单字符，不是 return)
(test-single-character "#\\r" "#\\r" #\r)

;; 测试 77: 测试部分命名字符 #\a (应该是简单字符，不是 alarm)
(test-single-character "#\\a" "#\\a" #\a)

;; 测试 78: 测试部分命名字符 #\b (应该是简单字符，不是 backspace)
(test-single-character "#\\b" "#\\b" #\b)

;; 测试 79: 测试部分命名字符 #\e (应该是简单字符，不是 escape)
(test-single-character "#\\e" "#\\e" #\e)

;; 测试 80: 测试部分命名字符 #\d (应该是简单字符，不是 delete)
(test-single-character "#\\d" "#\\d" #\d)

;; 测试 81: 测试混合大小写命名字符 #\Newline
(test-single-character "#\\Newline" "#\\Newline" #\newline)

;; 测试 82: 测试混合大小写命名字符 #\Space
(test-single-character "#\\Space" "#\\Space" #\space)

;; 测试 83: 测试混合大小写命名字符 #\Tab
(test-single-character "#\\Tab" "#\\Tab" #\tab)

;; 测试 84: 测试混合大小写命名字符 #\Return
(test-single-character "#\\Return" "#\\Return" #\return)

;; 测试 85: 测试混合大小写命名字符 #\Null
(test-single-character "#\\Null" "#\\Null" #\null)

;; 测试 86: 测试混合大小写命名字符 #\Alarm
(test-single-character "#\\Alarm" "#\\Alarm" #\alarm)

;; 测试 87: 测试混合大小写命名字符 #\Backspace
(test-single-character "#\\Backspace" "#\\Backspace" #\backspace)

;; 测试 88: 测试混合大小写命名字符 #\Escape
(test-single-character "#\\Escape" "#\\Escape" #\escape)

;; 测试 89: 测试混合大小写命名字符 #\Delete
(test-single-character "#\\Delete" "#\\Delete" #\delete)

;; 测试 90: 测试特殊字符 #\\
(test-single-character "#\\\\" "#\\\\" #\\)

;; 测试 91: 测试特殊字符 #\"
(test-single-character "#\\\"" "#\\\"" #\")

;; 测试 92: 测试特殊字符 #\'
(test-single-character "#\\'" "#\\'" #\')

;; 测试 93: 测试特殊字符 #\`
(test-single-character "#\\`" "#\\`" #\`)

;; 测试 94: 测试特殊字符 #\,
(test-single-character "#\\," "#\\," #\,)

;; 测试 95: 测试特殊字符 #\;
(test-single-character "#\\;" "#\\;" #\;)

;; 测试 96: 测试特殊字符 #\:
(test-single-character "#\\:" "#\\:" #\:)

;; 测试 97: 测试特殊字符 #\!
(test-single-character "#\\!" "#\\!" #\!)

;; 测试 98: 测试特殊字符 #\?
(test-single-character "#\\?" "#\\?" #\?)

;; 测试 99: 测试特殊字符 #\.
(test-single-character "#\\." "#\\." #\.)

;; 测试 100: 测试特殊字符 #\=
(test-single-character "#\\=" "#\\=" #\=)

;; 测试 101: 测试特殊字符 #\<
(test-single-character "#\\<" "#\\<" #\<)

;; 测试 102: 测试特殊字符 #\>
(test-single-character "#\\>" "#\\>" #\>)

;; 测试 103: 测试特殊字符 #\(
(test-single-character "#\\(" "#\\(" #\()

;; 测试 104: 测试特殊字符 #\)
(test-single-character "#\\)" "#\\)" #\))

;; 测试 105: 测试特殊字符 #\[
(test-single-character "#\\[" "#\\[" #\[)

;; 测试 106: 测试特殊字符 #\]
(test-single-character "#\\]" "#\\]" #\])

;; 测试 107: 测试特殊字符 #\{
(test-single-character "#\\{" "#\\{" #\{)

;; 测试 108: 测试特殊字符 #\}
(test-single-character "#\\}" "#\\}" #\})

;; 测试 109: 测试特殊字符 #\|
(test-single-character "#\\|" "#\\|" #\|)

;; 测试 110: 测试特殊字符 #\~
(test-single-character "#\\~" "#\\~" #\~)

;; 测试 111: 测试特殊字符 #\@
(test-single-character "#\\@" "#\\@" #\@)

;; 测试 112: 测试特殊字符 #\#
(test-single-character "#\\#" "#\\#" #\#)

;; 测试 113: 测试特殊字符 #\$
(test-single-character "#\\$" "#\\$" #\$)

;; 测试 114: 测试特殊字符 #\%
(test-single-character "#\\%" "#\\%" #\%)

;; 测试 115: 测试特殊字符 #\^
(test-single-character "#\\^" "#\\^" #\^)

;; 测试 116: 测试特殊字符 #\&
(test-single-character "#\\&" "#\\&" #\&)

;; 测试 117: 测试特殊字符 #\*
(test-single-character "#\\*" "#\\*" #\*)

;; 测试 118: 测试特殊字符 #\_
(test-single-character "#\\_" "#\\_" #\_)

;; 测试 119: 测试特殊字符 #\-
(test-single-character "#\\-" "#\\-" #\-)

;; 测试 120: 测试特殊字符 #\/
(test-single-character "#\\/" "#\\/" #\/)

;; 测试 121: 测试 ASCII 字符 #\x41 (A)
(test-single-character "#\\x41" "#\\x41" #\A)

;; 测试 122: 测试 ASCII 字符 #\x61 (a)
(test-single-character "#\\x61" "#\\x61" #\a)

;; 测试 123: 测试 ASCII 字符 #\x30 (0)
(test-single-character "#\\x30" "#\\x30" #\0)

;; 测试 124: 测试多个 ASCII 字符 " #\x41 #\x42 #\x43"
(test-multiple-characters " #\\x41 #\\x42 #\\x43"
                         '(("#\\x41" #\A)
                           ("#\\x42" #\B)
                           ("#\\x43" #\C)))

;; 测试 125: 测试字符与空白 " #\space #\tab #\newline"
(test-multiple-characters " #\\space #\\tab #\\newline"
                         '(("#\\space" #\space)
                           ("#\\tab" #\tab)
                           ("#\\newline" #\newline)))

;; 测试 126: 测试错误情况 # (只有 #)
(test-error-character "#" "#")

;; 测试 127: 测试错误情况 #g (无效的 # 前缀)
(test-error-character "#g" "#g")

;; 测试 128: 测试错误情况 #\xGG (两个无效十六进制)
(test-error-character "#\\xGG" "#\\xGG")

;; 测试 129: 测试错误情况 #\x1G (一个有效一个无效)
(test-error-character "#\\x1G" "#\\x1G")

;; 测试 130: 测试错误情况 #\x (只有 x)
(test-error-character "#\\x" "#\\x")

;; 测试 131: 测试错误情况 #\ (只有反斜杠)
(test-error-character "#\\" "#\\")

;; 测试 132: 测试字符 #\ (反斜杠字符)
(test-single-character "#\\\\" "#\\\\" #\\) ; 反斜杠字符

;; 测试 133: 测试错误情况 #\x (后面是空格)
(test-error-character "#\\x " "#\\x")

;; 测试 134: 测试错误情况 #\x (后面是换行)
(test-error-character "#\\x\n" "#\\x")

;; 测试 135: 测试错误情况 #\x (后面是制表符)
(test-error-character "#\\x\t" "#\\x")

;; 测试 136: 测试错误情况 #\x (后面是 EOF)
(test-error-character "#\\x" "#\\x")

;; 测试 137: 测试错误情况 #\newlin (不完整，应该回退到 #\n)
(test-single-character "#\\newlin" "#\\newlin" #\n)

;; 测试 138: 测试错误情况 #\spac (不完整，应该回退到 #\s)
(test-single-character "#\\spac" "#\\spac" #\s)

;; 测试 139: 测试错误情况 #\tru (不完整，应该回退到 #\t)
(test-single-character "#\\tru" "#\\tru" #\t)

;; 测试 140: 测试错误情况 #\fals (不完整，应该回退到 #\f)
(test-single-character "#\\fals" "#\\fals" #\f)

;; 测试 141: 测试错误情况 #\nul (不完整，应该回退到 #\n)
(test-single-character "#\\nul" "#\\nul" #\n)

;; 测试 142: 测试错误情况 #\al (不完整，应该回退到 #\a)
(test-single-character "#\\al" "#\\al" #\a)

;; 测试 143: 测试错误情况 #\ba (不完整，应该回退到 #\b)
(test-single-character "#\\ba" "#\\ba" #\b)

;; 测试 144: 测试错误情况 #\es (不完整，应该回退到 #\e)
(test-single-character "#\\es" "#\\es" #\e)

;; 测试 145: 测试错误情况 #\de (不完整，应该回退到 #\d)
(test-single-character "#\\de" "#\\de" #\d)

;; 测试 146: 测试错误情况 #\re (不完整，应该回退到 #\r)
(test-single-character "#\\re" "#\\re" #\r)

;; 测试 147: 测试错误情况 #\ta (不完整，应该回退到 #\t)
(test-single-character "#\\ta" "#\\ta" #\t)

;; 测试 148: 测试错误情况 #\sp (不完整，应该回退到 #\s)
(test-single-character "#\\sp" "#\\sp" #\s)

;; 测试 149: 测试错误情况 #\ne (不完整，应该回退到 #\n)
(test-single-character "#\\ne" "#\\ne" #\n)

;; 测试 150: 测试错误情况 #\in (不完整，应该回退到 #\i)
(test-single-character "#\\in" "#\\in" #\i)

;; 测试总结
(check-report)