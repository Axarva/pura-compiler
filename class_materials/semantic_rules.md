#### **Type Checking Rules (型チェック規則)**

* **`+`, `-`, `*`, `/` Operators**  
  * **Corresponding Syntax**: `<multiplicative_expression>`, `<additive_expression>`  
  * **Condition**: Both operands must be of type `Int`.  
  * **Result**: The result is of type `Int`.  
* **`==`, `!=` Operators**  
  * **Corresponding Syntax**: `<comparison_expression>`  
  * **Condition**: Both operands must be of the same type (e.g., `Int` and `Int`, `String` and `String`).  
  * **Result**: The result is of type `Bool`.  
* **`<`, `>`, `<=`, `>=` Operators**  
  * **Corresponding Syntax**: `<comparison_expression>`  
  * **Condition**: Both operands must be of type `Int`.  
  * **Result**: The result is of type `Bool`.  
* **`&&`, `||` Operators**  
  * **Corresponding Syntax**: `<or_expression>`, `<and_expression>`  
  * **Condition**: Both operands must be of type `Bool`.  
  * **Result**: The result is of type `Bool`.  
* **`++` Operator**  
  * **Corresponding Syntax**: `<concat_expression>`  
  * **Condition**: Both operands must be of type `String`.  
  * **Result**: The result is of type `String`.  
* **`!` Operator**  
  * **Corresponding Syntax**: `<unary_expression>`  
  * **Condition**: The operand must be of type `Bool`.  
  * **Result**: The result is of type `Bool`.  
* **Function Calls**  
  * **Corresponding Syntax**: `<function_call>`  
  * **Condition**: The number of arguments and their types must match the corresponding function's declared type signature.  
  * **Result**: The result is the function's declared return type.  
* **List Literals**  
  * **Corresponding Syntax**: `<list_literal>`  
  * **Condition**: All elements within the list must be of the same type.  
  * **Result**: The result is a `List` type of the element type (e.g., `List Int`).  
* **Function Definitions**  
  * **Corresponding Syntax**: `<function_definition>`  
  * **Condition**: The inferred type of the function body must match the declared return type in the function's type signature.  
  * **Result**: The function itself has the declared type signature.

#### **Effect Checking Rules (副作用チェック規則)**

* **`REQUIRES` Clause**  
  * **Corresponding Syntax**: `<function_definition>`  
  * **Condition**: Every side effect used within the function's body must be explicitly listed in the `REQUIRES` clause.  
  * **Example**: Using `print` requires `ConsoleWrite` to be declared.

