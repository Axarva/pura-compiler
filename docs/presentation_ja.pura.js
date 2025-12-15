const PuraRuntime = {
  elem: (tag) => (attrs) => (children) => ({ tag, attrs, children, key: null }),
  text: (str) => ({ tag: 'TEXT_NODE', text: String(str) }),
  on: (eventName) => (msg) => ({ type: 'event', name: eventName, msg: msg }),
  attr: (name) => (val) => ({ type: 'attribute', name: name, value: val }),
  print: (str) => console.log(str)
};


const initialModel = ["0", "微積試験勉強！！"];
const changeSlide = function(model) { return function(delta) { return (((currentIdx) => (((newIdx) => (((finalIdx) => (item => list => [item, ...list])(String(finalIdx))((list => list.slice(1))(model)))((((newIdx < 0)) ? (0) : ((((newIdx > 12)) ? (12) : (newIdx)))))))((currentIdx + delta))))(parseInt((list => list[0])(model)))); }; };
const addTodo = function(model) { return (((input) => (((input === "")) ? (model) : ((item => list => [item, ...list])((list => list[0])(model))((item => list => [item, ...list])(input)((list => list.slice(1))(model))))))(((msg) => { let r = window.prompt(msg); return r === null ? "" : r; })("新しいタスク名:"))); };
const removeTop = function(model) { return (((slideIdx) => (((todos) => (((list => list.length === 0)(todos)) ? (model) : ((item => list => [item, ...list])(slideIdx)((list => list.slice(1))(todos)))))((list => list.slice(1))(model))))((list => list[0])(model))); };
const mapTodos = function(f) { return function(list) { return (((list => list.length === 0)(list)) ? ([]) : ((item => list => [item, ...list])(f((list => list[0])(list)))(mapTodos(f)((list => list.slice(1))(list))))); }; };
const update = function(msg) { return function(model) { return (((msg === "NEXT")) ? (changeSlide(model)(1)) : ((((msg === "PREV")) ? (changeSlide(model)((0 - 1))) : ((((msg === "ADD_TODO")) ? (addTodo(model)) : ((((msg === "DEL_TODO")) ? (removeTop(model)) : (model)))))))); }; };
const slideWrapper = function(content) { return PuraRuntime.elem('div')([PuraRuntime.attr('class')("slide-container")])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("slide")])(content), PuraRuntime.elem('div')([PuraRuntime.attr('class')("nav-controls")])([PuraRuntime.elem('button')([PuraRuntime.on('click')("PREV"), PuraRuntime.attr('class')("nav-btn")])([PuraRuntime.text("←")]), PuraRuntime.elem('button')([PuraRuntime.on('click')("NEXT"), PuraRuntime.attr('class')("nav-btn")])([PuraRuntime.text("→")])])]); };
const slideWrapperStable = function(content) { return PuraRuntime.elem('div')([PuraRuntime.attr('class')("slide-container")])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("slide no-anim")])(content), PuraRuntime.elem('div')([PuraRuntime.attr('class')("nav-controls")])([PuraRuntime.elem('button')([PuraRuntime.on('click')("PREV"), PuraRuntime.attr('class')("nav-btn")])([PuraRuntime.text("←")]), PuraRuntime.elem('button')([PuraRuntime.on('click')("NEXT"), PuraRuntime.attr('class')("nav-btn")])([PuraRuntime.text("→")])])]); };
const fragmentClass = function(isVisible) { return ((isVisible) ? ("fragment visible") : ("fragment")); };
const codeSpan = function(cls) { return function(txt) { return PuraRuntime.elem('div')([PuraRuntime.attr('class')(("inline-code " + cls))])([PuraRuntime.text(txt)]); }; };
const kw = function(t) { return codeSpan("kw")(t); };
const str = function(t) { return codeSpan("str")(t); };
const fn = function(t) { return codeSpan("fn")(t); };
const cmt = function(t) { return codeSpan("cmt")(t); };
const redCmt = function(t) { return codeSpan("cmt-red")(t); };
const norm = function(t) { return codeSpan("norm")(t); };
const line = function(content) { return PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-line")])(content); };
const viewTitle = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title main-title")])([PuraRuntime.text("Pura")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle main-subtitle")])([PuraRuntime.text("UI 指向関数型言語")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("title-info")])([PuraRuntime.elem('p')([])([PuraRuntime.text("情報メディア特別演習 I")]), PuraRuntime.elem('p')([])([PuraRuntime.text("202513228　ティムシナ　アサルヴァ")]), PuraRuntime.elem('p')([])([PuraRuntime.text("アドバイザー教員：中井 央")])])]); };
const viewProblemJS = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("課題：実行時の脆弱性")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("動的型付けが引き起こす静かなる失敗")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("two-column")])([PuraRuntime.elem('div')([])([PuraRuntime.elem('p')([])([PuraRuntime.text("JavaScript の場合 (実行時エラー)")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([kw("function"), norm(" calc(cart) {")]), line([norm("  "), kw("let"), norm(" total "), kw("="), norm(" 0;")]), line([norm("  "), kw("for"), norm(" ("), kw("let"), norm(" item "), kw("of"), norm(" cart.items) {")]), line([norm("    "), redCmt("// バグ: \"10\" + 0 = \"010\"")]), line([norm("    total "), kw("+="), norm(" item.price;")]), line([norm("  }")]), line([norm("  "), kw("return"), norm(" total;")]), line([norm("}")])])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("hidden-block")])([PuraRuntime.elem('p')([])([PuraRuntime.text(".")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([norm(" ")])])])])]); };
const viewProblemPura = function(idx) { return slideWrapperStable([PuraRuntime.elem('h1')([])([PuraRuntime.text("課題：実行時の脆弱性")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("動的型付けが引き起こす静かなる失敗")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("two-column")])([PuraRuntime.elem('div')([])([PuraRuntime.elem('p')([])([PuraRuntime.text("JavaScript の場合 (実行時エラー)")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([kw("function"), norm(" calc(cart) {")]), line([norm("  "), kw("let"), norm(" total "), kw("="), norm(" 0;")]), line([norm("  "), kw("for"), norm(" ("), kw("let"), norm(" item "), kw("of"), norm(" cart.items) {")]), line([norm("    "), redCmt("// バグ: \"10\" + 0 = \"010\"")]), line([norm("    total "), kw("+="), norm(" item.price;")]), line([norm("  }")]), line([norm("  "), kw("return"), norm(" total;")]), line([norm("}")])])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.elem('p')([])([PuraRuntime.text("一方、Pura で同じコードを書くと...")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([kw("let"), fn(" calc"), norm(" "), kw("="), norm(" total "), kw("=>"), norm(" price "), kw("=>")]), line([norm("  total "), kw("+"), norm(" price")])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("error-box")])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("error-title")])([PuraRuntime.text("🛑 Type Mismatch Error")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("error-msg")])([PuraRuntime.text("Cannot add types Int and String.")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("error-detail")])([PuraRuntime.text("  | total + price")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("error-detail")])([PuraRuntime.text("  |         ^^^^^ Expected Int, got String")])])])])]); };
const viewPreReveal = function(idx) { return slideWrapper([PuraRuntime.elem('div')([PuraRuntime.attr('class')("center-suspense")])([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("wait-text")])([PuraRuntime.text("実は...")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("wait-sub")])([PuraRuntime.text("お気づきでしょうか？")])])]); };
const viewReveal = function(idx) { return slideWrapper([PuraRuntime.elem('div')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title reveal-title")])([PuraRuntime.text("このプレゼン自体が Pura 製です。")]), PuraRuntime.elem('p')([])([PuraRuntime.text("MVU アーキテクチャを使った状態管理機能付きアプリです。")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block reveal-code")])([line([kw("let"), fn(" viewReveal"), norm(" "), kw("="), norm(" idx "), kw("=>")]), line([norm("  slideWrapper [")]), line([norm("    h1 [] [ text "), str("\"このプレゼン自体が...\""), norm(" ]")]), line([norm("  ]")])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("stats-container")])([PuraRuntime.elem('p')([])([PuraRuntime.text("JavaScript: 0行")]), PuraRuntime.elem('p')([])([PuraRuntime.text("Runtime Errors: 0")])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("repo-link-container")])([PuraRuntime.elem('a')([PuraRuntime.attr('href')("https://github.com/Axarva/pura-compiler/tree/main/class_materials/final"), PuraRuntime.attr('class')("repo-link")])([PuraRuntime.text("Check Source on GitHub")])])])]); };
const viewTodoItem = function(item) { return PuraRuntime.elem('li')([PuraRuntime.attr('class')("todo-item")])([PuraRuntime.text(item)]); };
const viewDemo = function(model) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("実証デモ")]), PuraRuntime.elem('p')([])([PuraRuntime.text("以下のアプリは Pura Runtime 上で動作しています。")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("todo-container")])([PuraRuntime.elem('ul')([])(mapTodos(viewTodoItem)((list => list.slice(1))(model)))]), PuraRuntime.elem('div')([])([PuraRuntime.elem('button')([PuraRuntime.on('click')("ADD_TODO"), PuraRuntime.attr('class')("btn-primary")])([PuraRuntime.text("+ タスク追加")]), PuraRuntime.text(" "), PuraRuntime.elem('button')([PuraRuntime.on('click')("DEL_TODO"), PuraRuntime.attr('class')("btn-secondary")])([PuraRuntime.text("先頭を完了")])])]); };
const viewComparisonJS = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("比較分析")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("two-column")])([PuraRuntime.elem('div')([])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("col-header")])([PuraRuntime.text("JavaScript (命令型)")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([kw("function"), fn(" sum"), norm("(list) {")]), line([norm("  "), kw("let"), norm(" total "), kw("="), norm(" 0;")]), line([norm("  "), kw("for"), norm(" ("), kw("let"), norm(" i"), kw("="), norm("0; i<list.length; i++) {")]), line([norm("    total "), kw("+="), norm(" list[i];")]), line([norm("  }")]), line([norm("  "), kw("return"), norm(" total;")]), line([norm("}")])])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("hidden-block")])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("col-header")])([PuraRuntime.text(".")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([norm(" ")])])])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("hidden-block")])([PuraRuntime.elem('p')([])([PuraRuntime.text(".")])])]); };
const viewComparisonPura = function(idx) { return slideWrapperStable([PuraRuntime.elem('h1')([])([PuraRuntime.text("比較分析")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("two-column")])([PuraRuntime.elem('div')([])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("col-header")])([PuraRuntime.text("JavaScript (命令型)")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([kw("function"), fn(" sum"), norm("(list) {")]), line([norm("  "), kw("let"), norm(" total "), kw("="), norm(" 0;")]), line([norm("  "), kw("for"), norm(" ("), kw("let"), norm(" i"), kw("="), norm("0; i<list.length; i++) {")]), line([norm("    total "), kw("+="), norm(" list[i];")]), line([norm("  }")]), line([norm("  "), kw("return"), norm(" total;")]), line([norm("}")])])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("col-header")])([PuraRuntime.text("Pura (宣言型)")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([kw("let"), fn(" sum"), norm(" "), kw("="), norm(" list "), kw("=>")]), line([norm("  "), kw("if"), norm(" isEmpty list "), kw("then")]), line([norm("    0")]), line([norm("  "), kw("else")]), line([norm("    head list "), kw("+"), norm(" sum (tail list)")])])])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.elem('p')([])([PuraRuntime.text("「手順」ではなく「定義」を書くことで、状態管理のバグを排除します。")])])]); };
const viewInference = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("Hindley-Milner 型推論")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("コンパイル時における正当性の保証")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')(fragmentClass((idx >= 1)))])([PuraRuntime.text("型推論 + 明示的契約：内部は完全推論、境界は明示的")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')(fragmentClass((idx >= 2)))])([PuraRuntime.text("主要型 (Principal Types)：最も汎用的な型を特定")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')(fragmentClass((idx >= 3)))])([PuraRuntime.text("型安全：型付けされたプログラムは不正な動作をしない")])])]); };
const viewEffectsStep = function(step) { return (((wrapper) => wrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("副作用の管理 (Effects)")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')("visible")])([PuraRuntime.text("JavaScript や Python とは異なり、副作用は明示的です。")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("visible")])([PuraRuntime.text("関数は必要な「権限」を宣言しなければなりません。")])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("effects-grid")])([PuraRuntime.elem('div')([PuraRuntime.attr('class')((((step >= 1)) ? ("effect-card visible") : ("effect-card hidden-block")))])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("col-header")])([PuraRuntime.text("副作用の例")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block small-code")])([line([cmt("-- 画面で文字を表示する外部世界へのアクセス")]), line([kw("let"), fn(" log"), norm(" "), kw("="), norm(" msg "), kw("=>"), norm(" filename "), kw("=>")]), line([fn("  write"), norm(" msg"), norm(" filename")]), line([fn("  print"), norm(" msg")])])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')((((step >= 2)) ? ("effect-card fragment visible") : ("effect-card hidden-block")))])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("col-header")])([PuraRuntime.text(" Pura の解決策")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block small-code")])([line([cmt("-- 権限を宣言した Pura コード")]), line([kw("let"), fn(" log"), norm(" "), kw("="), norm(" msg "), kw("=>"), norm(" filename "), kw("=>")]), line([fn("  write"), norm(" msg"), norm(" filename")]), line([fn("  print"), norm(" msg")]), line([kw("  REQUIRES"), norm(" ConsoleWrite, FileIO")])])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')((((step >= 1)) ? ("effect-card visible") : ("effect-card hidden-block")))])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block small-code")])([line([cmt("-- 国際的副作用")]), line([kw("let"), fn(" badLog"), norm(" "), kw("="), norm(" msg "), kw("=>"), norm(" filename "), kw("=>")]), line([fn("  write"), norm(" msg"), norm(" filename")]), line([fn("  launchMissiles"), norm(" ()")])])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')((((step >= 2)) ? ("effect-card fragment visible") : ("effect-card hidden-block")))])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block small-code")])([line([cmt("-- エフェクトが安全装置として働く例")]), line([kw("let"), fn(" badLog"), norm(" "), kw("="), norm(" msg "), kw("=>"), norm(" filename "), kw("=>")]), line([fn("  write"), norm(" msg"), norm(" filename")]), line([fn("  launchMissiles"), norm(" ()"), redCmt("  -- エラー：許可されていない関数の呼び出し")]), line([kw("  REQUIRES"), norm(" FileIO")])])])])]))((((step === 1)) ? (slideWrapper) : (slideWrapperStable)))); };
const viewRoadmap = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("今後の展望")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("代数的エフェクト (Algebraic Effects)")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("WebAssembly (Wasm) バックエンド")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("標準ライブラリの拡充")])])]); };
const viewSummary = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("まとめ")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle last-subtitle")])([PuraRuntime.text("関数型言語 Pura を作りました！")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("two-column")])([PuraRuntime.elem('div')([])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("col-header center-text")])([PuraRuntime.text("課題と解決")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("動的型付けで「静かなる失敗」排除")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("宣言的記述で状態管理バグを防ぐ")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("JavaScript より読みやすい表現")])])]), PuraRuntime.elem('div')([])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("col-header center-text")])([PuraRuntime.text("技術と実証")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("型推論とエフェクトシステムで安全性")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("MVU アーキテクチャでの UI 構築を実現")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("実証：このスライド自体 Pura 製アプリ")])])])]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("copyright")])([PuraRuntime.text("Built with Pura 2025")])]); };
const view = function(model) { return (((idx) => (((idx === 0)) ? (viewTitle(idx)) : ((((idx === 1)) ? (viewProblemJS(idx)) : ((((idx === 2)) ? (viewProblemPura(idx)) : ((((idx === 3)) ? (viewPreReveal(idx)) : ((((idx === 4)) ? (viewReveal(idx)) : ((((idx === 5)) ? (viewDemo(model)) : ((((idx === 6)) ? (viewComparisonJS(idx)) : ((((idx === 7)) ? (viewComparisonPura(idx)) : ((((idx === 8)) ? (viewInference(3)) : ((((idx === 9)) ? (viewEffectsStep(1)) : ((((idx === 10)) ? (viewEffectsStep(2)) : ((((idx === 11)) ? (viewRoadmap(idx)) : (viewSummary(idx))))))))))))))))))))))))))(parseInt((list => list[0])(model)))); };
const main = (() => { return PuraRuntime.print("Starting Presentation..."); })();


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
