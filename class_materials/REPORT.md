# Pura Compiler Development Report - July 23, 2025

This report documents the progress made in debugging and formalizing the Pura language parser and type checker.

-----

## 1\. Initial Bug Fix: Identifier Parsing

* **Problem:** The parser originally contained a single helper function, `isIdentifier`, which could only check if a token was of the type `TokIdentifier _`. It could not differentiate between specific keywords (like `"List"`, `"Int"`) and general variable names. This made it impossible to write parsers that could reliably match required keywords, such as `satisfy (isIdentifier "List")`.

* **Solution:** The parsing logic was strengthened by creating two distinct helper functions:
    1.  A new, more powerful `isIdentifier :: String -> L.Token -> Bool` was introduced. This function checks if a token is an identifier that matches a *specific* string, allowing for robust keyword parsing.
    2.  The original, general-purpose helper was renamed to `isIdentifier' :: L.Token -> Bool` to clearly signify its role in matching *any* identifier, typically for variable or parameter names.

* **Result:** This change provided the necessary precision to parse the language's syntax correctly, resolving the initial class of parsing failures and crashes.


-----

## 2\. Resolving Infinite Left-Recursion in Expression Parsing

After the initial fix, the parser would hang indefinitely on expressions involving multiple operators.

  * **Problem:** A critical **infinite left-recursion** was discovered between `parseUnaryExpr` and `parseConcatExpr`. The `parseUnaryExpr` function would fall back to calling `parseConcatExpr`, which in turn would immediately call `parseUnaryExpr` as its first step without consuming any input tokens. This created a circular dependency that caused the parser to loop forever.

  * **Solution:** The operator precedence chain was corrected to untangle this recursion. The `parseUnaryExpr` function was modified so that its fallback parser is `parseTerm`, which handles the highest-precedence language constructs (literals, variables, etc.). The `parseConcatExpr` was then correctly inserted into its proper place within the binary operator precedence cascade.

  * **Result:** This fix successfully resolved the infinite loop, allowing the parser to correctly process complex expressions and parse the entire source file.

-----

## 3\. Type Checker Validation

With the parser now fully functional, we were able to validate the next stage of the compiler: the type checker.

  * **Generated AST:** The parser successfully processed a sample `greet` function and produced the following Abstract Syntax Tree (AST):

    ```haskell
    [Function {
        funcName = "greet", 
        funcTypeSignature = TArr TString TString, 
        funcArgs = ["name"], 
        funcBody = Block [
            Concat (Concat (Concat (LitString "Hello, ") (Var "name")) (LitString "!")) (LitBool True)
        ], 
        funcEffects = [ConsoleWrite]
    }]
    ```

  * **Analysis:** The AST correctly represents the expression `"Hello, " ++ name ++ "!" ++ True`. The type checker then analyzed this tree and correctly identified a type mismatch in the outermost `Concat` operation.

  * **Conclusion:** The type checker successfully reported the error: `Type error: Cannot concatenate TString and TBool. Expected String.` This demonstrates that the type-checking stage is functioning as intended, catching logical errors in the source code.

-----

## 4\. Formal Grammar Documentation (BNF)

To improve the project's documentation and formalize our understanding of the language's structure, we studied formal grammar theory.

  * **Action:** We adopted **Backus-Naur Form (BNF)** as the formal notation for our language's syntax.

  * **Implementation:**

    1.  **Inline Documentation:** The `Parser.hs` file was annotated with comments above each major parser function, describing the specific BNF rule that the function implements.
    2.  **Formal Grammar File:** A complete `pura.bnf` file was created to serve as a high-level, formal blueprint for the entire language grammar.

-----

## 5\. Parsing Methodology

The parsing strategy used in this compiler has been formally identified.

  * **Methodology:** The compiler uses **LL Parsing**, a top-down, predictive parsing strategy.

  * **Implementation:** Specifically, the parser is a hand-written **Recursive Descent Parser**. This is most evident in the "precedence cascade" implementation for expressions, where `parseExpr` calls `parseOrExpr`, which in turn calls `parseAndExpr`, and so on down to `parseTerm`. This chain of recursive function calls is the defining characteristic of this parsing approach.

<!-- end list -->