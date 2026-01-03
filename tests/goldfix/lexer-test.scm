;;; lexer-test.scm - Goldfix 词法分析器单元测试

(import (liii check)
        (goldfix lexer))

;; 设置测试模式
(check-set-mode! 'report-failed)

;; 测试 hello 函数
(check (hello) => "world")

;; 生成测试报告
(check-report "Goldfix lexer hello 函数测试完成")