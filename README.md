# Compiling — 编译原理课程项目

一个用 Haskell 实现的**简单类型 λ 演算（STLC）**编译器/解释器，支持词法分析、语法分析、双向类型检查与惰性求值。

---

## 1. 项目背景

本项目是《编译原理》课程的实践作业，实现了一个面向教学的小型函数式语言。

### 语言特性

- **类型**：`Int`（整数）、`String`（字符串）、函数类型 `T1 -> T2`
- **表达式**：变量、字面量、λ 抽象、函数应用、`let`/`let rec` 绑定
- **内建函数**：
  - `addInt`、`subInt`、`mulInt`：整数算术运算
  - `eqInt`：整数相等比较（返回 1 或 0）
  - `ite`：条件分支（惰性选择分支）
  - `concatStr`：字符串拼接
  - `intToStr`：整数转字符串
- **入口约定**：源文件定义 `main :: String` 作为程序入口

### 项目侧重

本项目覆盖了编译器的**前端**和**中后端**核心环节，侧重前端设计：

- **前端**：词法分析（monadic lexer）、语法分析（LL 风格 parser combinator）
- **中端**：双向类型推导/检查（bidirectional type checking）
- **后端**：基于 thunk 的惰性求值器

---

## 2. 项目设计

### 2.1 输入输出

- **输入**：包含顶层定义的源文件
  ```haskell
  main :: String
  main = concatStr "Hello, " "World!"
  ```
- **输出**：`main` 函数的求值结果（字符串）打印到标准输出
- **错误输出**：词法/语法/类型/运行时错误信息（带源码位置）打印到标准错误

### 2.2 编译管线

```
源文件 Text
    │
    ▼
┌──────────────────────────────────────┐
│ 1. 词法分析 (Lexer)                  │
│    Text → [Located (Lexcial Text)]   │
│    字符流 → 带位置信息的词法单元流     │
└──────────────────────────────────────┘
    │
    ▼
┌──────────────────────────────────────┐
│ 2. 语法分析 (Parser)                 │
│    词法单元 → TopLevel 列表          │
│    (Anno / Bind)                     │
│    合成：嵌套 let 表达式              │
└──────────────────────────────────────┘
    │
    ▼
┌──────────────────────────────────────┐
│ 3. 双向类型检查 (TypeChecker)        │
│    推导/检查类型                     │
└──────────────────────────────────────┘
    │
    ▼
┌──────────────────────────────────────┐
│ 4. 惰性求值 (Interpreter)            │
│    基于 thunk 的 call-by-need        │
│    内置函数以 VNative 形式嵌入       │
│    输出 VString 并打印               │
└──────────────────────────────────────┘
```

### 2.3 各 Pass 说明

#### 词法分析 (Lexer)

- 基于 **effect handler** 的可组合词法分析器
- 支持行注释 `--`、块注释 `-* ... *-`
- 支持整数常量、字符串常量（双引号包围）
- 支持关键字：`let`、`in`、`rec`、`\`、`->`、`::`、`=`、`(`、`)`
- 标识符：字母或下划线开头，后接字母/数字/下划线
- 每个词法单元携带源码位置（行:列）


#### 语法分析 (Parser)

- 手写的 Parser Combinator，基于 effect system
- 文法概要：

```
Prog :: (Bind | Anno)*
Anno :: var :: Ty
Bind :: var = Exp | rec var = Exp
Exp  :: var | lit | (Exp)
      | \ var -> Exp | \ var :: Ty -> Exp
      | let Bind in Exp
      | Exp Exp    (左结合)
      | Exp :: Ty
Ty   :: Int | String | Ty -> Ty | (Ty) | var
```

- 支持带有类型标注的 let 绑定：`let rec fact :: Int -> Int = \n -> ...`
- 函数应用左结合：`f x y` 解析为 `(f x) y`
- 使用 `try` 组合子实现选择性回溯
- 错误信息包含源码位置和上下文栈

#### 类型检查 (TypeChecker)

- 实现 **Bidirectional Type Checking**（双向类型推导/检查）
- `infer` 模式：自底向上推导表达式的类型
- `check` 模式：自顶向下验证表达式是否具有期类型
- 变量、字面量、λ 抽象、函数应用、let/let rec 均有完整覆盖
- 类型错误包含位置信息和详细说明
- 内置函数类型预注册在初始上下文中

#### 求值 (Interpreter)

- 基于 **thunk 的惰性求值**（Call-by-Need）
- 值类型：
  - `VInt`、`VString`：基值
  - `VClosure`：λ 闭包（环境 + 项）
  - `VNative`：内建函数
  - `VThunk`：延迟计算（环境 + 项）
- 惰性求值确保：
  - 未使用的绑定不被强制求值
  - `ite` 选择的分支才被求值
  - 递归终止条件正确
- 内建函数参数同样以 thunk 传入，按需 force

### 2.4 效应栈 (Effect Stack)

项目基于 `heftia-effects` 库使用代数效应（Algebraic Effects）管理副作用：

```
ParserES      = [Stream LexerStream, Input Token,
                 MultiThrow (ParserError, Snap),
                 Ask Snap, State Snap,
                 Input (Located (Lexcial TextStream))]
LexerES       = [Stream TextStream, MultiThrow (LexerError, Position),
                 Ask Position, SourceViewer,
                 ParserST TextStream, Throw FatalError]
TyEff v       = [Local (TypingCtx v ()), Ask (TypingCtx v ()),
                 MultiThrow TypeError]
RuntimeEff    = [MultiThrow RuntimeError]
```

---

## 3. 实现情况

### 3.1 工作量

| 模块 | 文件 | 行数 | 功能 |
|------|------|------|------|
| `Ast` | `src/Ast.hs` | 192 | AST 数据类型定义 |
| `Lexer` | `src/Lexer.hs` | 317 | 词法分析器 |
| `Parser` | `src/Parser.hs` | 420+ | 语法分析器 + 顶层程序解析 |
| `TypeChecker` | `src/TypeChecker.hs` | 414 | 双向类型检查 |
| `Interpreter` | `src/Interpreter.hs` | 195 | 惰性求值器 |
| `Printer` | `src/Printer.hs` | 87 | 错误信息格式化输出 |
| `Effect` | `src/Effect.hs` | 382 | Parser Combinator 基础设施 |
| `Exception` | `src/Exception.hs` | 99 | 异常树处理 |
| `Stream` | `src/Stream.hs` | 110 | 流抽象 |
| `Text` | `src/Text.hs` | 78 | 文本工具 + 变量生成器 |
| `Utils` | `src/Utils.hs` | 177 | 位置、错误类型等工具 |
| `Main` | `app/Main.hs` | 137 | CLI 入口 + 编译管线 |
| 测试 | `test/*.hs` | ~600 | 各模块 + 集成测试 |

### 3.2 代码结构

```
Compiling/
├── app/
│   └── Main.hs              -- CLI 入口、编译管线、类型擦除
├── src/
│   ├── Ast.hs               -- AST 定义（Term、Typ、TypedTerm、LitO、TypeLitO）
│   ├── Lexer.hs             -- 词法分析器（LexerConfig、lexer、refineLexcial）
│   ├── Parser.hs            -- 语法分析器（parseExp、parseBind、parseProg、parseTy）
│   ├── TypeChecker.hs       -- 类型检查器（infer、check）
│   ├── Interpreter.hs       -- 求值器（eval、Value、Env、builtinEnv）
│   ├── Effect.hs            -- Parser Combinator (ParseEff、try、satisfy、observing 等)
│   ├── Exception.hs         -- 异常树 (MultiThrow、translateToTree)
│   ├── Stream.hs            -- 流抽象 (Stream effect、TokenClass)
│   ├── Printer.hs           -- 错误信息格式化 (printErrorForest、SourceViewer)
│   ├── Text.hs              -- IsText、TShow 类 + FreshVarGen
│   ├── Utils.hs             -- Position、Located、TextStream、LexerError
│   └── Lib.hs               -- 占位模块
├── test/
│   ├── Spec.hs              -- 测试入口
│   ├── LexerSpec.hs         -- 词法分析测试
│   ├── ParserSpec.hs        -- 语法分析测试
│   ├── TypeCheckerSpec.hs   -- 类型检查测试
│   └── InterpreterSpec.hs   -- 求值器测试 (含端到端集成测试)
├── Compiling.cabal
├── package.yaml
├── stack.yaml
└── README.md
```

### 3.3 AI 使用说明

本项目在以下环节使用了 AI 辅助：

- 中后端的实现
- 一部分测试样例的编写，CLI 的编写
- 项目报告文档

主要使用 Zed Agent + DeepSeek V4 Flash

经验教训：

Haskell + Hefty 的技术栈比较生僻，往往需要多次尝试才能解决掉一些最简单的语法问题。对于标准的功能实现，在测试约束下反倒相对顺利。


## 4. 达成效果

### 4.1 自动化测试

项目包含 107 个自动化测试用例（其中 106 个通过，1 个为预知的语法边界 case），覆盖：

| 测试类别 | 用例数 | 覆盖内容 |
|----------|--------|----------|
| 词法分析 | 17+ | 字符解析、整数解析（2/8/10/16进制）、行注释、复杂词法序列 |
| 语法分析 | 20+ | 变量、字面量、λ 抽象、函数应用、let 绑定、递归绑定、类型标注 |
| 类型检查 | 30+ | 字面量推导、变量查找、λ 推導、应用检查、let 推导、递归 let、复杂表达式、Parser 集成 |
| 求值器 | 30+ | 字面量求值、闭包、函数应用、内建函数、let 绑定、惰性求值、递归、错误处理、端到端测试 |

### 4.2 示例程序

#### Hello World
```haskell
main :: String
main = "Hello, Compiler!"
```
```bash
$ stack exec Compiling-exe hello.sysy
Hello, Compiler!
```

#### 字符串拼接
```haskell
main :: String
main = concatStr "Hello, " "World!"
```
```bash
$ stack exec Compiling-exe concat.sysy
Hello, World!
```

#### 整数转字符串
```haskell
main :: String
main = intToStr 42
```
```bash
$ stack exec Compiling-exe int.sysy
42
```

#### 阶乘（递归 + 惰性条件）
```haskell
main :: String
main = intToStr
  (let rec fact :: Int -> Int =
    \n -> ite (eqInt n 0) 1 (mulInt n (fact (subInt n 1)))
  in fact 10)
```
```bash
$ stack exec Compiling-exe fact.sysy
3628800
```

#### 递归求和
```haskell
main :: String
main = intToStr
  (let rec sum :: Int -> Int =
    \n -> ite (eqInt n 0) 0 (addInt n (sum (subInt n 1)))
  in sum 100)
```
```bash
$ stack exec Compiling-exe sum.sysy
5050
```

### 4.3 错误处理

所有阶段的错误均带有源码位置信息：

```haskell
$ cat type_err.sysy
main :: String
main = 42

$ stack exec Compiling-exe type_err.sysy
|
1 | main :: String
  |
  ...
Type error:
: TypeError "`main` must have type `String`, but got: Int"
```

## 5. 参考文献

1. **Pierce, B. C.** *Types and Programming Languages*. MIT Press, 2002.

2. **Megaparsec** — Monadic parser combinators library.
   - Parser combinator 设计的参考

3. **Heftia** — Extensible effects for Haskell.
   - 项目使用的代数效应库
