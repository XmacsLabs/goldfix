## 项目目标

goldfix 的目标是实现 Goldfish Scheme 的代码质量工具套件，包括：

1. **Linter** - 代码静态分析工具
   - 检测语法错误和潜在问题
   - 检查代码风格和最佳实践
   - 提供代码质量建议

2. **Fixer** - 代码自动修复工具
   - 自动修复检测到的问题
   - 提供代码重构建议
   - 支持批量修复操作

3. **Formatter** - 代码格式化工具
   - 统一代码格式和缩进
   - 标准化代码布局
   - 支持自定义格式化规则

## 解释器路径和调用方式

解释器位于：`/usr/share/liiilabs/plugins/goldfish/bin/goldfish`

调用时需要指定为 r7rs mode：
```bash
/usr/share/liiilabs/plugins/goldfish/bin/goldfish --mode r7rs [其他参数]
```

## 项目目录结构

- `goldfix/` - 代码目录
- `tests/` - 测试用例目录
- `devel/` - 开发者文档目录

## 测试运行方法

### 1. 使用测试脚本（推荐）
```bash
./bin/test
```

### 2. 直接运行单个测试
```bash
/usr/share/liiilabs/plugins/goldfish/bin/goldfish -m r7rs tests/goldfix/lexer-test.scm
```

测试文件位于 `tests/goldfix/lexer-test.scm`，基于 `(liii check)` 测试框架实现。
