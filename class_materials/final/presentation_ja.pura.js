const PuraRuntime = {
  elem: (tag) => (attrs) => (children) => ({ tag, attrs, children, key: null }),
  text: (str) => ({ tag: 'TEXT_NODE', text: String(str) }),
  on: (eventName) => (msg) => ({ type: 'event', name: eventName, msg: msg }),
  attr: (name) => (val) => ({ type: 'attribute', name: name, value: val }),
  print: (str) => console.log(str)
};


const initialModel = ["0", "聴衆に笑顔を向ける"];
const changeSlide = function(model) { return function(delta) { return (((currentIdx) => (item => list => [item, ...list])(String((currentIdx + delta)))((list => list.slice(1))(model)))(parseInt((list => list[0])(model)))); }; };
const addTodo = function(model) { return (((input) => (item => list => [item, ...list])((list => list[0])(model))((item => list => [item, ...list])(input)((list => list.slice(1))(model))))(window.prompt("新しいタスクを入力してください:"))); };
const removeTop = function(model) { return (((slideIdx) => (((todos) => (((list => list.length === 0)(todos)) ? (model) : ((item => list => [item, ...list])(slideIdx)((list => list.slice(1))(todos)))))((list => list.slice(1))(model))))((list => list[0])(model))); };
const mapTodos = function(f) { return function(list) { return (((list => list.length === 0)(list)) ? ([]) : ((item => list => [item, ...list])(f((list => list[0])(list)))(mapTodos(f)((list => list.slice(1))(list))))); }; };
const update = function(msg) { return function(model) { return (((msg === "NEXT")) ? (changeSlide(model)(1)) : ((((msg === "PREV")) ? (changeSlide(model)((0 - 1))) : ((((msg === "ADD_TODO")) ? (addTodo(model)) : ((((msg === "DEL_TODO")) ? (removeTop(model)) : (model)))))))); }; };
const slideWrapper = function(content) { return PuraRuntime.elem('div')([PuraRuntime.attr('class')("slide-container")])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("slide")])(content)]); };
const fragmentClass = function(isVisible) { return ((isVisible) ? ("fragment visible") : ("fragment")); };
const viewTitle = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("Pura")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("UI指向関数型言語")]), PuraRuntime.elem('p')([])([PuraRuntime.text("JavaScriptより読みやすく、『型』でミスを防ぐ")])]); };
const viewProblem = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("課題：Web開発は『脆い』")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("些細なミスが、アプリ全体を壊します。")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([PuraRuntime.text("const user = null;\\nconsole.log(user.name);\\n// -> Uncaught TypeError: Crash!")])]); };
const viewTodoItem = function(item) { return PuraRuntime.elem('li')([PuraRuntime.attr('class')("todo-item")])([PuraRuntime.text(item)]); };
const viewDemo = function(model) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("デモ：堅牢なアプリ")]), PuraRuntime.elem('p')([])([PuraRuntime.text("Puraで作られた、安全なTodoリストです。")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("todo-container")])([PuraRuntime.elem('ul')([])(mapTodos(viewTodoItem)((list => list.slice(1))(model)))]), PuraRuntime.elem('div')([])([PuraRuntime.elem('button')([PuraRuntime.on('click')("ADD_TODO"), PuraRuntime.attr('class')("btn-primary")])([PuraRuntime.text("追加")]), PuraRuntime.text(" "), PuraRuntime.elem('button')([PuraRuntime.on('click')("DEL_TODO"), PuraRuntime.attr('class')("btn-secondary")])([PuraRuntime.text("完了")])])]); };
const viewComparison = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("解決策：圧倒的な可読性")]), PuraRuntime.elem('p')([])([PuraRuntime.text("JavaScript (ノイズが多い)")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block"), PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("function add(x, y) {\\n  return x + y;\\n}")]), PuraRuntime.elem('p')([])([PuraRuntime.text("Pura (ロジックのみ)")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block"), PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("let add = x => y => x + y")])]); };
const viewInference = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("解決策：HM型推論")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("コンパイラは、あなたの専属アシスタントです。")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')(fragmentClass((idx >= 1)))])([PuraRuntime.text("推論：型を書かなくても理解する")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')(fragmentClass((idx >= 2)))])([PuraRuntime.text("安全：実行前にミスを修正")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')(fragmentClass((idx >= 3)))])([PuraRuntime.text("数学的：Hindley-Milner アルゴリズム")])])]); };
const viewEffects = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("独自性：副作用の管理")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("「勝手な振る舞い」は許しません。")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([PuraRuntime.text("let main = { ... } REQUIRES ConsoleWrite")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("バグの温床（通信、書き換え）を制御")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("許可（Permission）ベースの安全性")])])]); };
const viewReveal = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("実は...")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle"), PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("このスライド自体が、Puraで動いています。")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("（HTMLもJSも書いていません！）")])]); };
const viewRoadmap = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("今後の展望")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("Koka言語の影響：代数的エフェクト")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("より柔軟な制御フローとテスト容易性")])])]); };
const viewEnd = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("ご清聴ありがとうございました")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("footer")])([PuraRuntime.text("Built with Pura Compiler 2025")])]); };
const view = function(model) { return (((idx) => (((idx === 0)) ? (viewTitle(idx)) : ((((idx === 1)) ? (viewProblem(idx)) : ((((idx === 2)) ? (viewDemo(model)) : ((((idx === 3)) ? (viewComparison(idx)) : ((((idx === 4)) ? (viewInference(3)) : ((((idx === 5)) ? (viewEffects(idx)) : ((((idx === 6)) ? (viewReveal(idx)) : ((((idx === 7)) ? (viewRoadmap(idx)) : (viewEnd(idx))))))))))))))))))(parseInt((list => list[0])(model)))); };
const main = (() => { return PuraRuntime.print("Starting Pura Presentation (JP)..."); })();


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
