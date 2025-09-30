# The Pura Programming Language

## 1. Introduction

Pura is a statically-typed, functional programming language designed with the goal of enabling reliable and predictable UI development. It compiles to JavaScript, allowing it to run in any modern web environment.

The language draws inspiration from Haskell and Elm, focusing on:
* **Purity and Immutability:** Encouraging a programming style that minimizes side effects.
* **Strong, Static Typing:** Catching errors at compile time before the program runs.
* **First-Class Functions:** Supporting higher-order functions, currying, and partial application.
* **Explicit Effects:** Making all interactions with the outside world (like console I/O) explicit and safe through an effect system.

This document serves as a brief manual for the current version of the Pura language.

---

## 2. Getting Started: The Compiler

The Pura compiler is a command-line tool written in Haskell.

### How to Build

This project is built using the Haskell `stack` tool.

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/Axarva/pura-compiler.git
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

### Compilation
To compile a Pura source file (e.g., `program.pura`), use the following command:

```stack exec pura-compiler program.pura```

### Execution
You can run the generated JavaScript file using Node.js:

```node program.pura.js```

---

## 3. Language Manual

### 3.1. Comments

Pura supports single-line comments starting with `--`.

```-- This is a comment. The compiler will ignore it.```

### 3.2. Types

Pura has a strong, static type system. All top-level functions require an explicit type declaration.

**Primitive Types:**
* `Int`: An integer number (e.g., `42`, `-10`).
* `String`: A sequence of characters (e.g., `"Hello, Pura!"`).
* `Bool`: A boolean value, either `True` or `False`.
* `Unit`: A type with only one value, `()`, used to represent the absence of a specific value. It is often the return type of functions that only perform side effects.

**Compound Types:**
* **Lists:** A homogenous list of elements, written as `List T` where `T` is another type.
    * `List Int`: A list of integers.
    * `List (String -> String)`: A list of functions.
* **Functions:** Function types are written using `->`. Functions are automatically curried.
    * `Int -> String`: A function that takes an `Int` and returns a `String`.
    * `Int -> String -> Bool`: A function that takes an `Int`, returns a new function that takes a `String`, which then returns a `Bool`.

### 3.3. Functions

All top-level functions must have a type declaration and a corresponding definition using `let`.

**Syntax:**
```
-- Type Declaration
functionName : Arg1Type -> Arg2Type -> ReturnType

-- Definition
let functionName = arg1 => arg2 => ... body ...
```
**Example:** A function that adds two integers.
```
add : Int -> Int -> Int
let add = x => y => x + y
```
### 3.4. Expressions

**Literals:**
* `123` (Int)
* `"hello"` (String)
* `True`, `False` (Bool)
* `()` (Unit)
* `[1, 2, 3]` (List Int)

**Operators:**
* **Arithmetic:** `+`, `-`, `*`, `/` (for `Int`)
* **String Concatenation:** `++`
* **Logical:** `&&` (and), `||` (or), `!` (not)
* **Comparison:** `==`, `!=`, `<`, `>`, `<=`, `>=`

**Conditional Logic: `if-then-else`**
The `if-then-else` construct is an expression that must evaluate to a value. The `then` and `else` branches must have the same type.

-- An expression that evaluates to a String
let message = if score > 90 then "Excellent" else "Good"

**Blocks: `{...}`**
A block allows you to sequence multiple statements. Each statement must end with a semicolon `;`. The block evaluates to the value of its final expression.

let main = {
  print "Step 1";
  -- Note: Local 'let' is not yet supported.
  print (add 5 10);
}

**Function Application (Calling Functions):**
Function application is done by writing expressions next to each other, separated by spaces. It is left-associative.

-- `add 5 10` is parsed as `(add 5) 10`
let result = add 5 10

-- Parentheses are used for grouping
let result = add (2 * 3) 10

### 3.5. The Effect System

Pura uses an effect system to make side effects explicit. Any function that interacts with the outside world (e.g., printing to the console) must declare the effects it uses.

**Syntax:**
A function definition is followed by the `REQUIRES` keyword and a comma-separated list of effects.

**Supported Effects:**
* `ConsoleWrite`: Allows the function to print to the console.

**Example:**
```
main : Unit
let main = {
  print "Hello, World!";
} REQUIRES ConsoleWrite
```
If a function calls `print` but does not declare `REQUIRES ConsoleWrite`, the compiler will produce an error.
 

This feature lays the foundation for future support of point-free style, higher-order UI abstractions, and first-class composition, which are key to the overall goal of easing UI development using this language.

### 3.6. A Slightly Bigger Program

The following program prints out a pine tree onto the console. 
```hs
-- tree-symmetrical.pura
-- Final version with no local 'let' bindings.

-- Type Declarations
makeString : Int -> String -> String
drawTreeRow : Int -> Int -> Unit
drawTree : Int -> Int -> Unit
main : Unit

-- A pure helper function that repeats a character string 'n' times.
let makeString = n => char =>
  if n <= 0 then
    ""
  else
    char ++ (makeString (n - 1) char)

-- An impure helper that prints a single, centered row of the tree.
let drawTreeRow = indent => stars =>
  print ((makeString indent " ") ++ (makeString stars "* ")) REQUIRES ConsoleWrite

-- An impure, recursive function that draws the tree from the top.
let drawTree = size => maxWidth =>
  if size <= maxWidth then {
    -- Substitute the 'let' bindings directly into the function call
    drawTreeRow (maxWidth - size) size;
    drawTree (size + 1) maxWidth;
  } else {
    ();
  }

-- The main entry point.
let main = {
  drawTree 1 10;
  drawTreeRow 8 2;
  drawTreeRow 8 2;
} REQUIRES ConsoleWrite
```
