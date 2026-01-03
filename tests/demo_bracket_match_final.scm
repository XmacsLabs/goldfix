;;; demo_bracket_match_final.scm - 括号匹配检测工具（最终版）
;;; 使用 (goldfix lexer) 进行分词，然后使用 (liii stack) 检测括号匹配问题

(import (scheme base)
        (scheme file)
        (liii stack)
        (goldfix lexer))

;; 读取文件内容为行列表
(define (read-file-lines file-path)
  (call-with-input-file file-path
    (lambda (port)
      (let loop ((lines '())
                 (line (read-line port)))
        (if (eof-object? line)
            (reverse lines)
            (loop (cons line lines) (read-line port)))))))

;; 读取文件内容为字符串
(define (read-file-content file-path)
  (call-with-input-file file-path
    (lambda (port)
      (let loop ((content "")
                 (line (read-line port)))
        (if (eof-object? line)
            content
            (loop (string-append content line "\n")
                  (read-line port)))))))

;; 括号匹配检测器
(define (check-bracket-matching file-path)
  (let* ((content (read-file-content file-path))
         (lexer (make-lexer content))
         (stack (stack :empty))
         (errors '())
         (bracket-count 0))

    ;; 辅助函数：添加错误信息
    (define (add-error! msg token stack-index)
      (set! errors (cons (list msg
                               (token-line token)
                               (token-column token)
                               (token-lexeme token)
                               stack-index)
                         errors)))

    ;; 主循环：读取所有 token
    (let loop ((token (lexer-next-token lexer))
               (position 0))
      (cond
        ;; 遇到 EOF 结束
        ((eof-token? token)
         ;; 检查栈中是否还有未匹配的左括号
         (let check-stack ((remaining (stack :to-list))
                           (index 0))
           (if (not (null? remaining))
               (let* ((stack-item (car remaining))
                      (token (car stack-item))
                      (stack-pos (cdr stack-item)))
                 (add-error! "未匹配的左括号" token stack-pos)
                 (check-stack (cdr remaining) (+ index 1))))))

        ;; 左括号：入栈，记录位置
        ((left-paren-token? token)
         (set! stack (stack :push (cons token bracket-count)))
         (set! bracket-count (+ bracket-count 1))
         (loop (lexer-next-token lexer) (+ position 1)))

        ;; 右括号：检查匹配
        ((right-paren-token? token)
         (if (= (stack :size) 0)
             (begin
               (add-error! "未匹配的右括号" token -1)
               (loop (lexer-next-token lexer) (+ position 1)))
             (begin
               (set! stack (stack :pop))
               (loop (lexer-next-token lexer) (+ position 1)))))

        ;; 其他 token：继续处理
        (else
         (loop (lexer-next-token lexer) (+ position 1)))))

    ;; 返回错误列表和统计信息
    (list errors
          bracket-count
          (stack :size))))

;; 显示带上下文的错误信息
(define (display-errors-with-context errors total-brackets unmatched-count file-path)
  (if (null? errors)
      (begin
        (display "✓ 括号匹配正确\n")
        (display (string-append "  总括号数: " (number->string total-brackets) "\n")))
      (begin
        (display "✗ 发现括号匹配错误：\n\n")
        (let ((lines (read-file-lines file-path)))
          (for-each (lambda (error)
                      (let ((msg (car error))
                            (line-num (cadr error))
                            (column-num (caddr error))
                            (lexeme (cadddr error))
                            (index (car (cddddr error))))

                        ;; 显示错误基本信息
                        (display (string-append "错误 [" (number->string index) "]: " msg "\n"))
                        (display (string-append "位置: 第 " (number->string line-num)
                                               " 行, 第 " (number->string column-num) " 列\n"))

                        ;; 计算上下文行范围
                        (let* ((total-lines (length lines))
                               (start-line (max 1 (- line-num 5)))
                               (end-line (min total-lines (+ line-num 5)))
                               (context-lines (list-tail lines (- start-line 1))))

                          ;; 显示上下文
                          (display "上下文:\n")
                          ;; 显示上方分隔线
                          (display "----------------\n")

                          (let loop ((current-line start-line)
                                     (remaining context-lines)
                                     (lines-shown 0))
                            (when (and (<= current-line end-line)
                                       (not (null? remaining)))
                              (let ((line-text (car remaining)))
                                ;; 显示内容（不显示行号）
                                (display line-text)
                                (display "\n")

                                ;; 如果是错误行，显示^标记
                                (when (= current-line line-num)
                                  ;; 计算正确的空格数量，使^对准括号位置
                                  (let ((spaces-needed (- column-num 1)))
                                    (display (make-string spaces-needed #\space))
                                    (display "^\n")))

                                (loop (+ current-line 1)
                                      (cdr remaining)
                                      (+ lines-shown 1)))))

                          ;; 显示下方分隔线
                          (display "----------------\n")

                          (display "\n"))))
                    (reverse errors)))

        (display (string-append "统计信息:\n"
                               "  总括号数: " (number->string total-brackets) "\n"
                               "  未匹配括号数: " (number->string unmatched-count) "\n"
                               "  错误总数: " (number->string (length errors)) "\n")))))

;; 主函数：检测指定文件的括号匹配
(define (main . args)
  (let ((file-path (if (null? args)
                       "tests/resources/200_14_bad.scm"
                       (car args))))
    (display (string-append "检测文件: " file-path "\n"))
    (display "========================================\n\n")

    (let ((result (check-bracket-matching file-path)))
      (let ((errors (car result))
            (total-brackets (cadr result))
            (unmatched-count (caddr result)))
        (display-errors-with-context errors total-brackets unmatched-count file-path)

        (if (> (length errors) 0)
            (display "\n建议：请检查括号是否成对出现。\n")
            (display "\n✓ 文件括号匹配正确。\n"))))))

;; 测试多个文件
(display "=== 测试简单文件 ===\n")
(main "tests/resources/simple_test.scm")

(display "\n=== 测试括号匹配错误的文件 ===\n")
(main "tests/resources/200_14_bad.scm")

(display "\n=== 测试长文件 ===\n")
(main "tests/resources/long_test.scm")

(display "\n=== 测试括号匹配正确的文件 ===\n")
(main "tests/resources/good_parens.scm")

(display "\n=== 测试自定义测试文件 ===\n")
(main "tests/resources/test_parens.scm")