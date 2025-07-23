 The Pura Compiler

Pura is a statically-typed, functional programming language prototype written in Haskell. This compiler implements a complete pipeline from source code to a final executable, including a lexer, a recursive descent parser, a type checker, and an effect system.

## Features

* **Static Typing:** All types are checked at compile time to prevent runtime type errors.
* **First-Class Functions:** Functions can be passed as arguments and returned from other functions.
* **Algebraic Data Types:** The language supports basic types like `Int`, `String`, `Bool`, `Unit`, and parameterized `List` types.
* **Effect System:** Functions must declare their side effects (e.g., `ConsoleWrite`), which are checked by the compiler.
* **Operator Precedence:** Mathematical and logical expressions are parsed with standard operator precedence.
* **Formal Grammar:** The language syntax is formally documented using Backus-Naur Form (BNF).

## How to Build

This project is built using the Haskell `stack` tool.

1.  **Clone the repository:**
    ```bash
    git clone <your-repo-url>
    cd pura-compiler
    ```

2.  **Build the project:**
    ```bash
    stack build
    ```
    This command will download the necessary GHC version and all dependencies, then compile the project.

3.  **Run the compiler:**
    The executable will be placed in the `.stack-work` directory. You can run it via `stack`:
    ```bash
    stack exec pura-compiler-exe -- <path-to-your-file.pura>
    ```

## Example Pura Code

Here is a small example of what Pura code looks like:

```haskell
-- All top-level functions require a type signature.
greet : String -> String

-- A function definition with a parameter list, body, and declared effects.
let greet = (name) => {
    "Hello, " ++ name ++ "!"
} REQUIRES ConsoleWrite

-- The main entry point is not yet implemented, but functions can be defined.
```

## Project Structure

The compiler is organized into several modules, each with a specific responsibility:

* `Main.hs`: The main entry point for the compiler executable. It reads the file, runs the compilation pipeline, and prints the results or errors.
* `Lexer.hs`: The **Lexer** (or tokenizer). It scans the raw source code string and converts it into a stream of tokens (e.g., `TokIdentifier`, `TokPlus`).
* `Parser.hs`: The **Parser**. It takes the stream of tokens from the lexer and builds an Abstract Syntax Tree (AST) based on the language's grammar. It is implemented as a top-down, recursive descent parser.
* `AST.hs`: Defines the **Abstract Syntax Tree**. These are the Haskell data structures that represent the code's structure (e.g., `Function`, `Expr`, `BinOp`).
* `Types.hs`: Defines the data structures for the language's **type system** (e.g., `TInt`, `TArr`).
* `TypeChecker.hs`: The **Type Checker**. It walks the AST to verify that all expressions and function calls adhere to the language's type rules, preventing type errors.
* `Permissions.hs`: The **Effect Checker**. It walks the AST to ensure that any function performing a side effect has explicitly declared it in its `REQUIRES` clause.
* `CodeGen.hs`: (Future Work) This module will be responsible for **Code Generation**, taking the validated AST and translating it into a target language like LLVM or another high-level language.
