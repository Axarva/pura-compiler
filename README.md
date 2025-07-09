
# Pura Compiler — Current Status and Roadmap

## 1. What the compiler does now

- **Lexer**:  
  Converts raw source code text into a stream of tokens like `TokLet`, `TokIdentifier`, `TokStringLiteral`, `TokStrConcat`, etc.

- **Parser**:  
  Transforms token stream into an Abstract Syntax Tree (AST).  
  Currently supports:  
  - Function definitions (`let greet = (name) => { ... } REQUIRES ...`)  
  - Expressions: string literals, variables, function calls, string concatenations (`++`), do blocks, and blocks `{ ... }`  
  - Optional effect annotations (`REQUIRES ConsoleWrite, FileIO`)

- **AST**:  
  Data types representing functions, expressions, and effects for further processing.

---

## 2. How the greet function is parsed

Source:

```pura
let greet = (name) => {
  "Hello, " ++ name ++ "!"
} REQUIRES ConsoleWrite
```

### Token stream (output of lexer):

```
[TokLet, TokIdentifier "greet", TokEquals, TokLParen, TokIdentifier "name", TokRParen, TokArrow, TokLBrace,
 TokStringLiteral "Hello, ", TokStrConcat, TokIdentifier "name", TokStrConcat, TokStringLiteral "!", TokRBrace,
 TokRequires, TokIdentifier "ConsoleWrite", TokEOF]
```

### Parser workflow:

- `parseFunction` expects the `let` keyword → parses the function name → parses parameters → parses arrow `=>` → parses function body block → optionally parses the `REQUIRES` clause.  
- `parseBlock` collects expressions inside `{ ... }`.  
- `parseExpr` parses concatenation expressions, recursively building nested `Concat` AST nodes for `++`.  
- `parseTerm` recognizes string literals, variables, function calls, or do blocks.  
- `parseEffectName` maps identifiers like `"ConsoleWrite"` to the `Effect` data type.

### Final AST:

```haskell
Function
  { funcName = "greet"
  , funcArgs = ["name"]
  , funcBody = Block
      [ Concat
          (LitString "Hello, ")
          (Concat (Var "name") (LitString "!"))
      ]
  , funcEffects = [ConsoleWrite]
  }
```

---

## 3. Compiler Architecture Roadmap

### Pipeline overview:

```
Source code
  ↓ Lexer
Tokens
  ↓ Parser
AST
  ↓ Type Checker (verify types, e.g., string concatenation validity)
  ↓ Effect Checker (verify declared vs used side effects)
  ↓ Code Generator (target JS, bytecode, or other)
```

### Module responsibilities:

- **Lexer** (`Lexer.hs`): tokenizes source text.  
- **Parser** (`Parser.hs`): builds AST from tokens.  
- **AST** (`AST.hs`): defines data types for functions, expressions, and effects.  
- **TypeChecker** (`TypeChecker.hs`): enforces type rules.  
- **EffectChecker** (`EffectChecker.hs`): verifies side effect declarations.  
- **CodeGen** (`CodeGen.hs`): outputs target code.

---

## 4. Next steps

- Implement basic **type checking** for expressions and functions.  
- Implement **effect checking**:  
  - Detect unused or undeclared effects.  
  - Warn or error accordingly.  
- Expand language syntax (numbers, conditionals, recursion).  
- Start prototype **code generation** for a simple target (e.g., JS or Haskell).  
- Add test cases and improve error reporting.