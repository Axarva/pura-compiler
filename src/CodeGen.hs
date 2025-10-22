module CodeGen where

import AST
import Types

mapBuiltin :: String -> String
mapBuiltin name = case name of
  -- HTML Elements
  "div"    -> "PuraRuntime.elem('div')"
  "p"      -> "PuraRuntime.elem('p')"
  "button" -> "PuraRuntime.elem('button')"
  "h1"     -> "PuraRuntime.elem('h1')"
  -- Text Node
  "text"   -> "PuraRuntime.text"
  -- Event Handlers
  "onClick" -> "PuraRuntime.on('click')"
  -- Utility Functions
  "toString" -> "String"
  "print"    -> "PuraRuntime.print"
  -- Default Case: it's a user-defined variable
  _        -> name


generateExpr :: Expr -> String
generateExpr expr = case expr of
  -- Literals to string representation.
  LitInt n      -> show n
  LitBool b     -> if b then "true" else "false"
  LitString s   -> show s
  LitUnit     -> "null" 

  -- A variable in Pura is a variable in JS.
  Var name -> mapBuiltin name

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
    -- case reverse exprs of
    --   [] -> "(() => {})()" -- An empty block is a no-op
    --   (lastExpr:rest) ->
    --     let bodyStmts = map (\e -> generateExpr e ++ ";") (reverse rest)
    --     in "(() => { " ++ concat bodyStmts ++ "return " ++ generateExpr lastExpr ++ "; })()"
    let stmts = map (\e -> generateExpr e ++ ";") exprs
    in "(() => { " ++ concat stmts ++ "})()"

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


generateProgram :: [Function] -> String
generateProgram funcs =
  let compiledFunctions = unlines (map generateFunction funcs)
      runtime = puraRuntimeJS
      mainLoop = mainLoopJS
  in runtime ++ "\n\n" ++ compiledFunctions ++ "\n\n" ++ mainLoop

puraRuntimeJS :: String
puraRuntimeJS = unlines
  [ "const PuraRuntime = {"
  , "  elem: (tag) => (attrs) => (children) => ({ tag, attrs, children, key: null }),"
  , "  text: (str) => ({ tag: 'TEXT_NODE', text: String(str) }),"
  , "  on: (eventName) => (msg) => ({ type: 'event', name: eventName, msg: msg }),"
  , "  print: (str) => console.log(str)"
  , "};"
  ]

mainLoopJS :: String
mainLoopJS = unlines
  [ "// --- MVU Main Loop ---"
  , "function mount(selector, program) {"
  , "  const root = document.querySelector(selector);"
  , "  let model = program.initialModel;"
  , ""
  , "  const dispatch = (msg) => {"
  , "    model = program.update(msg)(model);"
  , "    render();"
  , "  };"
  , ""
  , "  function renderNode(vnode) {"
  , "    if (vnode.tag === 'TEXT_NODE') {"
  , "      return document.createTextNode(vnode.text);"
  , "    }"
  , "    const el = document.createElement(vnode.tag);"
  , "    vnode.attrs.forEach(attr => {"
  , "      if (attr.type === 'event') {"
  , "        el.addEventListener(attr.name, () => dispatch(attr.msg));"
  , "      }"
  , "      // Add other attribute types here (e.g., className)"
  , "    });"
  , "    vnode.children.forEach(child => {"
  , "      el.appendChild(renderNode(child));"
  , "    });"
  , "    return el;"
  , "  }"
  , ""
  , "  function render() {"
  , "    const newView = program.view(model);"
  , "    root.innerHTML = ''; // Simple and inefficient, but works for a demo!"
  , "    root.appendChild(renderNode(newView));"
  , "  }"
  , ""
  , "  render();"
  , "}"
  , ""
  , "function safeMount() {"
  , "  const hasMVU = typeof view !== 'undefined' && typeof update !== 'undefined' && typeof initialModel !== 'undefined';"
  , "  if (hasMVU) {"
  , "    mount('#app', { initialModel, update, view });"
  , "  } else {"
  , "    //console.log('Running as a script...');"
  , "    if (typeof main === 'function') main();"
  , "  }"
  , "}"
  , ""
  , "safeMount();"
  ]