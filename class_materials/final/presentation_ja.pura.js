const PuraRuntime = {
  elem: (tag) => (attrs) => (children) => ({ tag, attrs, children, key: null }),
  text: (str) => ({ tag: 'TEXT_NODE', text: String(str) }),
  on: (eventName) => (msg) => ({ type: 'event', name: eventName, msg: msg }),
  attr: (name) => (val) => ({ type: 'attribute', name: name, value: val }),
  print: (str) => console.log(str)
};


const initialModel = ["0", "微積試験勉強！！"];
const changeSlide = function(model) { return function(delta) { return (((currentIdx) => (((newIdx) => (((finalIdx) => (item => list => [item, ...list])(String(finalIdx))((list => list.slice(1))(model)))((((newIdx < 0)) ? (0) : ((((newIdx > 8)) ? (8) : (newIdx)))))))((currentIdx + delta))))(parseInt((list => list[0])(model)))); }; };
const addTodo = function(model) { return (((input) => (item => list => [item, ...list])((list => list[0])(model))((item => list => [item, ...list])(input)((list => list.slice(1))(model))))(window.prompt("新しいタスク名:"))); };
const removeTop = function(model) { return (((slideIdx) => (((todos) => (((list => list.length === 0)(todos)) ? (model) : ((item => list => [item, ...list])(slideIdx)((list => list.slice(1))(todos)))))((list => list.slice(1))(model))))((list => list[0])(model))); };
const mapTodos = function(f) { return function(list) { return (((list => list.length === 0)(list)) ? ([]) : ((item => list => [item, ...list])(f((list => list[0])(list)))(mapTodos(f)((list => list.slice(1))(list))))); }; };
const update = function(msg) { return function(model) { return (((msg === "NEXT")) ? (changeSlide(model)(1)) : ((((msg === "PREV")) ? (changeSlide(model)((0 - 1))) : ((((msg === "ADD_TODO")) ? (addTodo(model)) : ((((msg === "DEL_TODO")) ? (removeTop(model)) : (model)))))))); }; };
const slideWrapper = function(content) { return PuraRuntime.elem('div')([PuraRuntime.attr('class')("slide-container")])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("slide")])(content), PuraRuntime.elem('div')([PuraRuntime.attr('class')("nav-controls")])([PuraRuntime.elem('button')([PuraRuntime.on('click')("PREV"), PuraRuntime.attr('class')("nav-btn")])([PuraRuntime.text("←")]), PuraRuntime.elem('button')([PuraRuntime.on('click')("NEXT"), PuraRuntime.attr('class')("nav-btn")])([PuraRuntime.text("→")])])]); };
const fragmentClass = function(isVisible) { return ((isVisible) ? ("fragment visible") : ("fragment")); };
const codeSpan = function(cls) { return function(txt) { return PuraRuntime.elem('div')([PuraRuntime.attr('class')(("inline-code " + cls))])([PuraRuntime.text(txt)]); }; };
const kw = function(t) { return codeSpan("kw")(t); };
const str = function(t) { return codeSpan("str")(t); };
const fn = function(t) { return codeSpan("fn")(t); };
const cmt = function(t) { return codeSpan("cmt")(t); };
const norm = function(t) { return codeSpan("norm")(t); };
const line = function(content) { return PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-line")])(content); };
const viewTitle = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("Pura")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("UI指向関数型言語")]), PuraRuntime.elem('p')([])([PuraRuntime.text("JavaScriptより読みやすく、「型」でミスを防ぐ。")])]); };
const viewProblem = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("課題：実行時の脆弱性")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("防御的コーディングによる認知的負荷")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([kw("function"), norm(" processUserData(user) {")]), line([norm("  "), kw("let"), norm(" result "), kw("="), kw(" null"), norm(";")]), line([norm("  "), cmt("// 過剰なネストによる防御")]), line([norm("  "), kw("if"), norm(" (user "), kw("&&"), norm(" user.meta) {")]), line([norm("    "), cmt("// バグ：比較(===)ではなく代入(=)になっている")]), line([norm("    "), kw("if"), norm(" (user.role "), kw("="), str(" 'admin'"), norm(") {")]), line([norm("      "), kw("if"), norm(" ("), kw("typeof"), norm(" user.score "), kw("!=="), str(" 'undefined'"), norm(") {")]), line([norm("        result "), kw("="), norm(" user.score * 2;")]), line([norm("      }")]), line([norm("    }")]), line([norm("  }")]), line([norm("  "), cmt("// クラッシュの可能性：resultがnullの場合")]), line([norm("  "), kw("return"), norm(" result.toString();")]), line([norm("}")])]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("たった一つのミスが、本番環境を停止させます。")])]); };
const viewTodoItem = function(item) { return PuraRuntime.elem('li')([PuraRuntime.attr('class')("todo-item")])([PuraRuntime.text(item)]); };
const viewDemo = function(model) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("実証デモ")]), PuraRuntime.elem('p')([])([PuraRuntime.text("以下のアプリは Pura Runtime 上で動作しています。")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("todo-container")])([PuraRuntime.elem('ul')([])(mapTodos(viewTodoItem)((list => list.slice(1))(model)))]), PuraRuntime.elem('div')([])([PuraRuntime.elem('button')([PuraRuntime.on('click')("ADD_TODO"), PuraRuntime.attr('class')("btn-primary")])([PuraRuntime.text("+ タスク追加")]), PuraRuntime.text(" "), PuraRuntime.elem('button')([PuraRuntime.on('click')("DEL_TODO"), PuraRuntime.attr('class')("btn-secondary")])([PuraRuntime.text("先頭を完了")])])]); };
const viewComparison = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("比較分析")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("two-column")])([PuraRuntime.elem('div')([])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("col-header")])([PuraRuntime.text("JavaScript (命令型)")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([kw("function"), fn(" sum"), norm("(list) {")]), line([norm("  "), kw("let"), norm(" total "), kw("="), norm(" 0;")]), line([norm("  "), kw("for"), norm(" ("), kw("let"), norm(" i"), kw("="), norm("0; i<list.length; i++) {")]), line([norm("    total "), kw("+="), norm(" list[i];")]), line([norm("  }")]), line([norm("  "), kw("return"), norm(" total;")]), line([norm("}")])])]), PuraRuntime.elem('div')([])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("col-header")])([PuraRuntime.text("Pura (宣言型)")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([kw("let"), fn(" sum"), norm(" "), kw("="), norm(" list "), kw("=>")]), line([norm("  "), kw("if"), norm(" isEmpty list "), kw("then")]), line([norm("    0")]), line([norm("  "), kw("else")]), line([norm("    head list "), kw("+"), norm(" sum (tail list)")])])])]), PuraRuntime.elem('p')([])([PuraRuntime.text("再帰を用いることで、数学的な正しさが保証されます。")])]); };
const viewInference = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("Hindley-Milner 型推論")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("コンパイル時における正当性の保証")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')(fragmentClass((idx >= 1)))])([PuraRuntime.text("Algorithm W：型注釈なしで型を導出")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')(fragmentClass((idx >= 2)))])([PuraRuntime.text("主要型 (Principal Types)：最も汎用的な型を特定")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')(fragmentClass((idx >= 3)))])([PuraRuntime.text("実行時エラーゼロ：型付けされたプログラムは誤作動しない")])])]); };
const viewEffects = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("副作用の管理 (Effects)")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("外部世界へのアクセスを明示的に制御")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("JavaScriptとは異なり、副作用は明示的です。")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("関数は必要な「権限」を宣言しなければなりません。")])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([cmt("-- 'main' 関数は実行に特定の権限を要求する")]), line([kw("let"), fn(" main"), norm(" "), kw("="), norm(" { ... }")]), line([kw("  REQUIRES"), norm(" ConsoleWrite, BrowserPrompt")])])]); };
const viewRoadmap = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("今後の展望")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("代数的エフェクト (Algebraic Effects)")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("WebAssembly (Wasm) バックエンド")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("標準ライブラリの拡充")])])]); };
const viewConclusion = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("結論")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("Puraは、厳密な型理論が")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("現代のインタラクティブなUI開発と共存できることを示しました。")])]); };
const viewEnd = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("ご清聴ありがとうございました")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("質疑応答")]), PuraRuntime.elem('p')([])([PuraRuntime.text("Built with Pura Compiler 2025")])]); };
const view = function(model) { return (((idx) => (((idx === 0)) ? (viewTitle(idx)) : ((((idx === 1)) ? (viewProblem(idx)) : ((((idx === 2)) ? (viewDemo(model)) : ((((idx === 3)) ? (viewComparison(idx)) : ((((idx === 4)) ? (viewInference(3)) : ((((idx === 5)) ? (viewEffects(idx)) : ((((idx === 6)) ? (viewRoadmap(idx)) : ((((idx === 7)) ? (viewConclusion(idx)) : (viewEnd(idx))))))))))))))))))(parseInt((list => list[0])(model)))); };
const main = (() => { return PuraRuntime.print("Starting University Rehearsal (JP)..."); })();


// --- MVU Main Loop ---
function mount(selector, program) {
  const root = document.querySelector(selector);
  let model = program.initialModel;

  const dispatch = (msg) => {
    model = program.update(msg)(model);
    render();
  };

  function renderNode(vnode) {
    if (vnode.tag === 'TEXT_NODE') {
      return document.createTextNode(vnode.text);
    }
    const el = document.createElement(vnode.tag);
    vnode.attrs.forEach(attr => {
      if (attr.type === 'event') {
        el.addEventListener(attr.name, () => dispatch(attr.msg));
      } else if (attr.type === 'attribute') {
        el.setAttribute(attr.name, attr.value);
      }
    });
    vnode.children.forEach(child => {
      el.appendChild(renderNode(child));
    });
    return el;
  }

  function render() {
    const newView = program.view(model);
    root.innerHTML = ''; // Simple and inefficient, but works for a demo!
    root.appendChild(renderNode(newView));
  }
  // --- KEYBOARD LISTENER ---
  document.addEventListener('keydown', (e) => {
    if (e.key === 'ArrowRight' || e.key === ' ') {
       dispatch("NEXT");
    }
    if (e.key === 'ArrowLeft') {
       dispatch("PREV");
    }
  });

  render();
}

function safeMount() {
  const hasMVU = typeof view !== 'undefined' && typeof update !== 'undefined' && typeof initialModel !== 'undefined';
  if (hasMVU) {
    mount('#app', { initialModel, update, view });
  } else {
    //console.log('Running as a script...');
    if (typeof main === 'function') main();
  }
}

safeMount();
