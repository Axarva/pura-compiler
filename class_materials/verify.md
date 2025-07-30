###Testing

* **1. Unit Tests for Each Compiler Stage**
    * **Purpose**: Verify individual compiler components function correctly in isolation.
    * **Methodology**:
        * **Lexer Testing**:
            * **Goal**: Ensure accurate tokenization and error handling.
            * **Test Cases**:
                * **Keywords**: `let` -> `TokLet`, `do` -> `TokDo`, `REQUIRES` -> `TokRequires`.
                * **Operators**: `=>` -> `TokArrow`, `->` -> `TokRightArrow`, `++` -> `TokStrConcat`, `==` -> `TokEqEq`, `!=` -> `TokBangEq`, `<=` -> `TokLtEq`, `>=` -> `TokGtEq`, `&&` -> `TokAmpAmp`, `||` -> `TokPipePipe`, `+` -> `TokPlus`, `-` -> `TokMinus`, `*` -> `TokStar`, `/` -> `TokSlash`, `!` -> `TokBang`.
                * **Literals**: `123` -> `TokNumber 123`, `"hello"` -> `TokStringLiteral "hello"`, `True` -> `TokBoolLiteral True`, `False` -> `TokBoolLiteral False`.
                * **Identifiers**: `greet` -> `TokIdentifier "greet"`, `myVar` -> `TokIdentifier "myVar"`, `List` -> `TokIdentifier "List"`.
                * **Punctuation**: `(`, `)`, `{`, `}`, `:`, `,`, `[`, `]`.
                * **Edge Cases**: Empty string, whitespace-only input, unexpected characters (e.g., `#`).
        * **Parser Testing**:
            * **Goal**: Verify correct AST construction for valid token streams; graceful failure for invalid ones.
            * **Test Cases**:
                * **Operator Precedence & Associativity**: `1 + 2 * 3` -> `1 + (2 * 3)` AST. `1 + 2 - 3` -> `(1 + 2) - 3` AST. `True || False && True` -> `True || (False && True)` AST. `"a" ++ "b" ++ "c"` -> `("a" ++ "b") ++ "c"` AST.
                * **Unary Operators**: `!True` -> `UnOp Not (LitBool True)`. `!!False` -> `UnOp Not (UnOp Not (LitBool False))`.
                * **Parenthesized Expressions**: `(1 + 2) * 3` -> `(1 + 2)` block/sub-expression evaluated first.
                * **Function Calls**: `func()`, `func(x)`, `func(1, "hello")`.
                * **Blocks**: Empty `{}`, single-expression `{ 1 + 2 }`, multi-expression `{ 1; 2; "hello" }`.
                * **Do Blocks**: `do {}`, `do { print("hi") }`.
                * **List Literals**: Empty `[]`, single-element `[1]`, multi-element `[1, 2 + 3]`.
                * **Type Parsing**: `Int`, `String -> Bool`, `Int -> String -> Unit`, `(Int -> Bool) -> String`, `List Int`, `List (String -> Bool)`.
                * **Top-Level Declarations**: `func : Int -> Int`, `let func = (x) => { x }`.
                * **Negative Tests (Syntax Errors)**: Missing parentheses (`1 + (2`), unclosed block (`let func = (x) => {`), incomplete type signature (`func : Int ->`).
        * **Type Checker Testing**:
            * **Goal**: Verify all type checking rules.
            * **Test Cases**:
                * **Valid Programs**: `1 + 2`, `"hello" ++ "world"`, `True && False`.
                * **Invalid Programs (Type Mismatches)**: `Int + String`, `Bool && Int`, `Int > String`, `String ++ Int`, `[1, "two"]`.
                * **Function Calls**: Correct arity and type matching. Incorrect arity or type mismatch (e.g., `add(1)` for `Int -> Int -> Int`, `add("one", "two")`).
        * **Effect Checker Testing**:
            * **Goal**: Verify `REQUIRES` clause correctness.
            * **Test Cases**:
                * **Valid**: Use `print` with `REQUIRES ConsoleWrite`.
                * **Invalid**: Use `print` *without* `REQUIRES ConsoleWrite`.

* **2. Integration Tests**
    * **Purpose**: Verify compiler pipeline components work together seamlessly.
    * **Test Case 1: Basic Arithmetic and Type Check**
        * **Goal**: Ensure `Int` literals, arithmetic ops, and type checking integrate.
        * **Input (`.pura` file)**:
            ```pura
            add : Int -> Int -> Int
            let add = (x) => (y) => {
              x + y
            }
            ```
        * **Expected Outcome**: Successful lexing, parsing, type checking, and effect checking.

    * **Test Case 2: String Concatenation with Type Error**
        * **Goal**: Confirm type checker identifies `String` and `Bool` mismatch.
        * **Input (`.pura` file)**:
            ```pura
            greet : String -> String
            let greet = (name) => {
              "Hello, " ++ name ++ "!" ++ True
            }
            ```
        * **Expected Outcome**: Successful lexing and parsing. **Failure** during type checking: "Type error: Cannot concatenate TString and TBool. Expected String."

    * **Test Case 3: Function Call and `REQUIRES` Clause**
        * **Goal**: Verify function call type and declared effect.
        * **Input (`.pura` file)**:
            ```pura
            greet : String -> Unit
            let greet = (name) => {
              print("Hello, " ++ name ++ "!")
            } REQUIRES ConsoleWrite
            ```
        * **Expected Outcome**: Successful lexing, parsing, type checking, and effect checking.

    * **Test Case 4: Missing `REQUIRES` Clause**
        * **Goal**: Ensure effect checker catches missing declaration.
        * **Input (`.pura` file)**:
            ```pura
            greet : String -> Unit
            let greet = (name) => {
              print("Hello, " ++ name ++ "!")
            }
            ```
        * **Expected Outcome**: Successful lexing, parsing, type checking. **Failure** during effect checking: "ConsoleWrite is a required but undeclared effect."

    * **Test Case 5: List Literal Type Check**
        * **Goal**: Verify list literal parsing and type checking.
        * **Input (`.pura` file)**:
            ```pura
            myNumbers : List Int
            let myNumbers = () => {
              [1, 2, 3]
            }

            mixedList : List String -> List String
            let mixedList = (x) => {
              ["apple", "banana", 1] -- Type error here
            }
            ```
        * **Expected Outcome**: Successful lexing and parsing of both functions. `myNumbers` passes type check (`List Int`). `mixedList` **fails** type check: "String and Int cannot coexist in the same list."

* **3. Acceptance Tests (End-to-End)**

    * **Purpose:** Verify the compiler's behavior for a wide range of scenarios, ensuring it meets the language specification for both valid and invalid programs.

    * **Methodology:**

        * **Pass Tests:**

            Goal: Ensure all valid Pura programs compile successfully.

            Method: Create a directory (pass_tests/) containing numerous valid Pura programs.

            Verification: Run the compiler on each file; confirm success status (e.g., exit code 0) and no error output.

        * **Fail Tests:**

            Goal: Verify the compiler correctly reports specific errors for invalid programs.

            Method: Create a directory (fail_tests/) containing programs with single, deliberate errors, categorized by type.

            Test Categories:

                fail_tests/syntax_error/: Files with incorrect grammar (e.g., missing parentheses, unclosed blocks). Compiler should fail during parsing.

                fail_tests/type_mismatch/: Files with incorrect types (e.g., 1 + "2", [1, True]). Compiler should fail during type checking with a specific error message.

                fail_tests/undeclared_effect/: Files using a side-effecting function without declaration (e.g., print without REQUIRES ConsoleWrite). Compiler should fail during effect checking.

            Verification: For each file, check compiler exit status (error) and confirm the error message contains expected text/code.
