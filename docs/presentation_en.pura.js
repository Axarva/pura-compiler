const PuraRuntime = {
  elem: (tag) => (attrs) => (children) => ({ tag, attrs, children, key: null }),
  text: (str) => ({ tag: 'TEXT_NODE', text: String(str) }),
  on: (eventName) => (msg) => ({ type: 'event', name: eventName, msg: msg }),
  attr: (name) => (val) => ({ type: 'attribute', name: name, value: val }),
  print: (str) => console.log(str)
};


const initialModel = ["0", "Smile at audience"];
const changeSlide = function(model) { return function(delta) { return (((currentIdx) => (((newIdx) => (((finalIdx) => (item => list => [item, ...list])(String(finalIdx))((list => list.slice(1))(model)))((((newIdx < 0)) ? (0) : ((((newIdx > 12)) ? (12) : (newIdx)))))))((currentIdx + delta))))(parseInt((list => list[0])(model)))); }; };
const addTodo = function(model) { return (((input) => (item => list => [item, ...list])((list => list[0])(model))((item => list => [item, ...list])(input)((list => list.slice(1))(model))))(window.prompt("Enter Task Name:"))); };
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
const norm = function(t) { return codeSpan("norm")(t); };
const line = function(content) { return PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-line")])(content); };
const viewTitle = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("Pura")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("A UI-Oriented Functional Language")]), PuraRuntime.elem('p')([])([PuraRuntime.text("Bridging Compiler Theory and Frontend Application.")])]); };
const viewProblem = function(step) { return (((wrapper) => wrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("Challenge: Runtime Instability")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("Dynamic typing creates silent failures.")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("two-column")])([PuraRuntime.elem('div')([])([PuraRuntime.elem('p')([])([PuraRuntime.text("JavaScript (Runtime Error)")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([kw("function"), norm(" calc(cart) {")]), line([norm("  "), kw("let"), norm(" total "), kw("="), norm(" 0;")]), line([norm("  "), kw("for"), norm(" ("), kw("let"), norm(" item "), kw("of"), norm(" cart.items) {")]), line([norm("    "), cmt("// BUG: \"10\" + 0 = \"010\"")]), line([norm("    total "), kw("+="), norm(" item.price;")]), line([norm("  }")]), line([norm("  "), kw("return"), norm(" total;")]), line([norm("}")])])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')(fragmentClass((step === 2)))])([PuraRuntime.elem('p')([])([PuraRuntime.text("Meanwhile, in Pura...")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([kw("let"), fn(" calc"), norm(" "), kw("="), norm(" total "), kw("=>"), norm(" price "), kw("=>")]), line([norm("  total "), kw("+"), norm(" price")])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("error-box")])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("error-title")])([PuraRuntime.text("🛑 Type Mismatch Error")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("error-msg")])([PuraRuntime.text("Cannot add types Int and String.")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("error-detail")])([PuraRuntime.text("  | total + price")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("error-detail")])([PuraRuntime.text("  |         ^^^^^ Expected Int, got String")])])])])]))((((step === 2)) ? (slideWrapperStable) : (slideWrapper)))); };
const viewPreReveal = function(idx) { return slideWrapper([PuraRuntime.elem('div')([PuraRuntime.attr('class')("center-suspense")])([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("wait-text")])([PuraRuntime.text("Actually...")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("wait-sub")])([PuraRuntime.text("Did you notice?")])])]); };
const viewReveal = function(idx) { return slideWrapper([PuraRuntime.elem('div')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title reveal-title")])([PuraRuntime.text("This presentation is built with Pura.")]), PuraRuntime.elem('p')([])([PuraRuntime.text("It is an application with state management using the MVU architecture.")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block reveal-code")])([line([norm(" ")]), line([kw("let"), fn(" viewReveal"), norm(" "), kw("="), norm(" idx "), kw("=>")]), line([norm("  slideWrapper [")]), line([norm("    h1 [] [ text "), str("\"This presentation...\""), norm(" ]")]), line([norm("  ]")])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("stats-container")])([PuraRuntime.elem('p')([])([PuraRuntime.text("JavaScript: 0 lines")]), PuraRuntime.elem('p')([])([PuraRuntime.text("Runtime Errors: 0")])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("repo-link-container")])([PuraRuntime.elem('a')([PuraRuntime.attr('href')("https://github.com/Axarva/pura-compiler/tree/main/docs"), PuraRuntime.attr('class')("repo-link")])([PuraRuntime.text("Check Source on GitHub")])])])]); };
const viewTodoItem = function(item) { return PuraRuntime.elem('li')([PuraRuntime.attr('class')("todo-item")])([PuraRuntime.text(item)]); };
const viewDemo = function(model) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("Demonstration")]), PuraRuntime.elem('p')([])([PuraRuntime.text("The following application is running within the Pura Runtime.")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("todo-container")])([PuraRuntime.elem('ul')([])(mapTodos(viewTodoItem)((list => list.slice(1))(model)))]), PuraRuntime.elem('div')([])([PuraRuntime.elem('button')([PuraRuntime.on('click')("ADD_TODO"), PuraRuntime.attr('class')("btn-primary")])([PuraRuntime.text("Add Task")]), PuraRuntime.text(" "), PuraRuntime.elem('button')([PuraRuntime.on('click')("DEL_TODO"), PuraRuntime.attr('class')("btn-secondary")])([PuraRuntime.text("Complete Top")])])]); };
const viewComparison = function(step) { return (((wrapper) => wrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("Comparative Analysis")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("two-column")])([PuraRuntime.elem('div')([])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("col-header")])([PuraRuntime.text("JavaScript (Imperative)")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([kw("function"), fn(" sum"), norm("(list) {")]), line([norm("  "), kw("let"), norm(" total "), kw("="), norm(" 0;")]), line([norm("  "), kw("for"), norm(" ("), kw("let"), norm(" i"), kw("="), norm("0; i<list.length; i++) {")]), line([norm("    total "), kw("+="), norm(" list[i];")]), line([norm("  }")]), line([norm("  "), kw("return"), norm(" total;")]), line([norm("}")])])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')(fragmentClass((step === 2)))])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("col-header")])([PuraRuntime.text("Pura (Declarative)")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([kw("let"), fn(" sum"), norm(" "), kw("="), norm(" list "), kw("=>")]), line([norm("  "), kw("if"), norm(" isEmpty list "), kw("then")]), line([norm("    0")]), line([norm("  "), kw("else")]), line([norm("    head list "), kw("+"), norm(" sum (tail list)")])])])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')(fragmentClass((step === 2)))])([PuraRuntime.elem('p')([])([PuraRuntime.text("By writing 'definitions' instead of 'steps', we eliminate state management bugs.")])])]))((((step === 2)) ? (slideWrapperStable) : (slideWrapper)))); };
const viewInference = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("Hindley-Milner Inference")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("Compile-time correctness guarantees.")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')(fragmentClass((idx >= 1)))])([PuraRuntime.text("Inference + Contracts: Internals fully inferred, boundaries explicit.")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')(fragmentClass((idx >= 2)))])([PuraRuntime.text("Principal Types: Finds the most general type possible.")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')(fragmentClass((idx >= 3)))])([PuraRuntime.text("Zero Runtime Errors: Well-typed programs cannot go wrong.")])])]); };
const viewEffects = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("Effect Management")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("Explicit control over side effects.")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("Unlike JavaScript, effects are explicit.")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("Functions must declare required permissions.")])]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([line([cmt("-- 'main' requires specific permissions to run")]), line([kw("let"), fn(" main"), norm(" "), kw("="), norm(" { ... }")]), line([kw("  REQUIRES"), norm(" ConsoleWrite, BrowserPrompt")])])]); };
const viewRoadmap = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("Future Work")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("Algebraic Effects (Koka-inspired control flow)")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("WebAssembly (Wasm) Compilation Target")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("Standard Library Expansion")])])]); };
const viewConclusion = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("Conclusion")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("Pura demonstrates that rigorous type theory can exist")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("alongside modern, interactive UI development.")])]); };
const viewEnd = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("Thank You")]), PuraRuntime.elem('p')([])([PuraRuntime.text("Built with Pura Compiler 2025")])]); };
const view = function(model) { return (((idx) => (((idx === 0)) ? (viewTitle(idx)) : ((((idx === 1)) ? (viewProblem(1)) : ((((idx === 2)) ? (viewProblem(2)) : ((((idx === 3)) ? (viewPreReveal(idx)) : ((((idx === 4)) ? (viewReveal(idx)) : ((((idx === 5)) ? (viewDemo(model)) : ((((idx === 6)) ? (viewComparison(1)) : ((((idx === 7)) ? (viewComparison(2)) : ((((idx === 8)) ? (viewInference(3)) : ((((idx === 9)) ? (viewEffects(idx)) : ((((idx === 10)) ? (viewRoadmap(idx)) : ((((idx === 11)) ? (viewConclusion(idx)) : (viewEnd(idx))))))))))))))))))))))))))(parseInt((list => list[0])(model)))); };
const main = (() => { return PuraRuntime.print("Starting University Presentation (EN)..."); })();


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
