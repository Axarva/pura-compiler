-- In CodeGen.hs
module CodeGen where

import AST
import Types

generateExpr :: Expr -> String
generateExpr expr = case expr of
  -- Literals to string representation.
  LitInt n      -> show n
  LitBool b     -> if b then "true" else "false"
  LitString s   -> show s

  -- A variable in Pura is a variable in JS.
  Var name      -> name

  -- Binary operators map directly to JS operators.
  -- Parentheses are crucial to preserve precedence!
  BinOp op e1 e2 ->
    let opStr = case op of
          Add -> "+"
          Sub -> "-"
          Mul -> "*"
          Div -> "/"
          And -> "&&"
          Or  -> "||"
          Eq  -> "===" -- Use strict equality in JS
          Neq -> "!=="
          Lt  -> "<"
          Gt  -> ">"
          Le  -> "<="
          Ge  -> ">="
    in "(" ++ generateExpr e1 ++ " " ++ opStr ++ " " ++ generateExpr e2 ++ ")"

  -- String concatenation also maps directly.
  Concat e1 e2 -> "(" ++ generateExpr e1 ++ " + " ++ generateExpr e2 ++ ")"

  -- Function application in Pura (f x y) is the same in JS,
  -- assuming functions are curried.
  Apply func arg -> generateExpr func ++ "(" ++ generateExpr arg ++ ")"
  
  -- A block returns the value of its last expression.
  -- An Immediately Invoked Function Expression (IIFE) in JS is a perfect fit.
  Block exprs ->
    case reverse exprs of
      [] -> "(() => {})()" -- An empty block is a no-op
      (lastExpr:rest) ->
        let bodyStmts = map (\e -> generateExpr e ++ ";") (reverse rest)
        in "(() => { " ++ concat bodyStmts ++ "return " ++ generateExpr lastExpr ++ "; })()"

  OpAsFunction op ->
    let opStr = case op of
          Add -> "+"
          Sub -> "-"
          Mul -> "*"
          Div -> "/"
          And -> "&&"
          Or  -> "||"
          Eq  -> "==="
          Neq -> "!=="
          Lt  -> "<"
          Gt  -> ">"
          Le  -> "<="
          Ge  -> ">="
      in "(a => b => a " ++ opStr ++ " b)"

  IfThenElse cond thenExpr elseExpr ->
    "((" ++ generateExpr cond ++ ") ? (" ++ generateExpr thenExpr ++ ") : (" ++ generateExpr elseExpr ++ "))"
    
      -- Other cases to add later...
  _ -> "/* unhandled AST node */"

generateFunction :: Function -> String
generateFunction (Function name _ args body _) =
  "const " ++ name ++ " = " ++ generateCurriedFunction args body ++ ";"

generateCurriedFunction :: [String] -> Expr -> String
generateCurriedFunction [] body = generateExpr body -- Base case: no more arguments
generateCurriedFunction (arg:rest) body =
  "function(" ++ arg ++ ") { return " ++ generateCurriedFunction rest body ++ "; }"