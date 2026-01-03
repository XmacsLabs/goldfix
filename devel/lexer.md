# Goldfix Lexer 设计文档

## 概述

Goldfix 的 Lexer 是代码质量工具套件的核心组件，它不仅负责词法分析，还要为后续的 Linter、Fixer 和 Formatter 提供丰富的上下文信息。每个 Token 必须携带足够的元数据以支持无损格式化和智能修复。

## Token 数据结构设计

### 核心设计原则

1. **无损保留**：保留原始代码的所有信息，包括空格、注释、原始字面量
2. **丰富上下文**：为每个 Token 提供完整的物理位置和格式化信息
3. **错误容忍**：即使遇到语法错误，也能生成有用的 Token 信息
4. **缩进感知**：准确记录缩进信息以支持括号修复

### Token 记录类型定义

```scheme
(define-record-type <token>
  (make-token type lexeme line column offset indent-level literal-value
              leading-whitespace is-terminated? has-error?)
  token?
  ;; 1. 核心属性
  (type          token-type)          ;; 符号：'LEFT_PAREN, 'IDENTIFIER, 'STRING, 等
  (lexeme        token-lexeme)        ;; 字符串：原始文本（必须保留原样）

  ;; 2. 物理位置参数
  (line          token-line)          ;; 整数：起始行号（1-based）
  (column        token-column)        ;; 整数：起始列号（1-based）
  (offset        token-offset)        ;; 整数：文件绝对位置（字符偏移量）
  (indent-level  token-indent)        ;; 整数：所在行的起始缩进量

  ;; 3. 关联数据
  (literal-value token-value)         ;; 任意：解析后的 Scheme 对象

  ;; 4. 格式化元数据
  (leading-whitespace token-leading-ws) ;; 字符串：该 Token 前面的空格（用于快速缩进分析）

  ;; 5. 扫描状态标志
  (is-terminated? token-terminated?)  ;; 布尔值：字符串/注释是否闭合
  (has-error?     token-has-error?))  ;; 布尔值：Token 内部是否有错误
```

### 为什么 `leading-whitespace` 如此重要？

在修复算法中，我们经常要问："当前这个 `(define` 是否对齐到了行首？"。如果每个 Token 都记录了它前面的空格数量，修复器就不需要反复回溯扫描文件，极大提高了处理不平衡括号时的搜索效率。

**关键优势：**
1. **快速缩进分析**：无需回溯即可知道 Token 的缩进级别
2. **括号修复优化**：在寻找缺失括号位置时，可以快速比较缩进
3. **格式保持**：在重新格式化代码时，可以保留原有的缩进风格
4. **错误检测**：可以检测缩进不一致的问题

**示例：**
```scheme
(define (foo)
  (if (> x 0)
    (display "ok")
  (display "done"))  ; 这里缩进回退了！
```

当 Lexer 扫描到 `(display "done")` 时，它会记录其 `leading-whitespace` 为 2 个空格（假设 `define` 是 0）。
Fixer 在处理 `if` 时，发现它的 `then` 分支缩进是 4，而当前 Token 的缩进突然变成了 2，这无需复杂的语法树分析，仅凭 **Token 的缩进参数** 就能断定 `if` 的作用域必须在前一行强制结束。

## Token 类型分类

为了让 Goldfix 能够完美地重建代码结构，Lexer 需要定义的 Token 列表比普通的编译器更细致。我们将 Token 分为四类：**结构化 Token**（用于理解结构）、**字面量与标识符**（原子数据）、**元数据与格式 Token**（用于保持格式）和**容错专用 Token**。

### 1. 结构化 Token (Structural)

这些是 Goldfix 识别 S-表达式边界的核心。

| Token 类型 | 示例 | 说明 |
| --- | --- | --- |
| `LEFT_PAREN` | `(` | 表达式开始 |
| `RIGHT_PAREN` | `)` | 表达式结束 |
| `VEC_START` | `#(` | R7RS 向量开始 |
| `DOT` | `.` | 用于处理不当列表 `(a . b)` |
| `QUOTE_CHAR` | `'` | 语法糖，等同于 `(quote ...)` |
| `QUASIQUOTE` | ``` | 反引号，准引用 |
| `UNQUOTE` | `,` | 逗号，解引用 |
| `UNQUOTE_SPLICING` | `,@` | 逗号+@，切片解引用 |

### 2. 字面量与标识符 (Atoms)

这些 Token 携带具体的值信息。

| Token 类型 | 示例 | 说明 |
| --- | --- | --- |
| `IDENTIFIER` | `define`, `if+`, ` | var with space` | 标识符，支持空格 |
| `NUMBER` | `42`, `#x2A`, `3.14` | 各种进制和格式的数字 |
| `STRING` | `"hello \"world\""` | 处理内部转义的字符串 |
| `BOOLEAN` | `#t`, `#f`, `#true`, `#false` | R7RS 支持的长短两种形式 |
| `CHARACTER` | `#\space`, `#\a` | 字符型字面量 |
| `BYTEVECTOR` | `#u8(1 2 3)` | R7RS 的字节向量标识 |

### 3. 元数据与格式 Token (Trivia)

**这是 Fixer 最关键的部分。** 传统的 `read` 会忽略它们，但 Goldfix 必须记录它们。

| Token 类型 | 示例 | 修复时的用途 |
| --- | --- | --- |
| `WHITESPACE` | `       ` (空格/Tab) | **核心：** 用于计算缩进，判断 `if/let` 边界 |
| `NEWLINE` | `\n`, `\r\n` | 标记物理行结束，重置列坐标 |
| `COMMENT_LINE` | `; comment` | 确保修复括号时不会插入到注释中间 |
| `COMMENT_BLOCK` | `# | ... | #` | 多行注释，需要特殊处理嵌套 |
| `DIRECTIVE` | `#!fold-case` | R7RS 读入器指令 |

### 4. 容错专用 Token

当 Lexer 遇到无法解析的内容时，不应报错，而是标记为特殊类型：

| Token 类型 | 说明 |
| --- | --- |
| `ILLEGAL` | 无法识别的字符（如非法转义） |
| `UNTERMINATED_STRING` | 到文件末尾还没闭合的字符串 |
| `EOF` | 文件结束符，携带"当前括号栈仍未清空"的元信息 |

## 行注释 (`;`) 处理

在 Scheme 中，`;` 用于行注释，注释从 `;` 开始直到行尾的所有内容。虽然行注释相对简单，但在无损格式化和修复中仍需特别注意。

### 处理规则

1. **识别注释开始**：遇到 `;` 字符时进入注释模式
2. **收集注释内容**：读取直到行尾（`\n` 或 `\r\n`）的所有字符
3. **生成注释 Token**：生成 `COMMENT_LINE` Token，包含完整的注释内容（包括 `;` 字符）
4. **位置信息**：准确记录注释的起始位置

### 特殊考虑

1. **空注释**：`;` 后面直接换行的情况
2. **Windows 换行**：正确处理 `\r\n`
3. **文件结束**：注释在文件末尾结束的情况
4. **Unicode 字符**：注释中可能包含 Unicode 字符

### 与修复器的集成

行注释在修复过程中需要特别注意：

1. **括号插入**：修复器不能在注释中间插入括号
2. **格式保持**：保留注释的原始缩进和位置
3. **错误检测**：检测注释是否被错误地分割
