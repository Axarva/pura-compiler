const PuraRuntime = {
  elem: (tag) => (attrs) => (children) => ({ tag, attrs, children, key: null }),
  text: (str) => ({ tag: 'TEXT_NODE', text: String(str) }),
  on: (eventName) => (msg) => ({ type: 'event', name: eventName, msg: msg }),
  attr: (name) => (val) => ({ type: 'attribute', name: name, value: val }),
  print: (str) => console.log(str)
};


const initialModel = ["0", "Smile at audience"];
const changeSlide = function(model) { return function(delta) { return (((currentIdx) => (item => list => [item, ...list])(String((currentIdx + delta)))((list => list.slice(1))(model)))(parseInt((list => list[0])(model)))); }; };
const addTodo = function(model) { return (((input) => (item => list => [item, ...list])((list => list[0])(model))((item => list => [item, ...list])(input)((list => list.slice(1))(model))))(window.prompt("Enter a new task:"))); };
const removeTop = function(model) { return (((slideIdx) => (((todos) => (((list => list.length === 0)(todos)) ? (model) : ((item => list => [item, ...list])(slideIdx)((list => list.slice(1))(todos)))))((list => list.slice(1))(model))))((list => list[0])(model))); };
const mapTodos = function(f) { return function(list) { return (((list => list.length === 0)(list)) ? ([]) : ((item => list => [item, ...list])(f((list => list[0])(list)))(mapTodos(f)((list => list.slice(1))(list))))); }; };
const update = function(msg) { return function(model) { return (((msg === "NEXT")) ? (changeSlide(model)(1)) : ((((msg === "PREV")) ? (changeSlide(model)((0 - 1))) : ((((msg === "ADD_TODO")) ? (addTodo(model)) : ((((msg === "DEL_TODO")) ? (removeTop(model)) : (model)))))))); }; };
const slideWrapper = function(content) { return PuraRuntime.elem('div')([PuraRuntime.attr('class')("slide-container")])([PuraRuntime.elem('div')([PuraRuntime.attr('class')("slide")])(content)]); };
const fragmentClass = function(isVisible) { return ((isVisible) ? ("fragment visible") : ("fragment")); };
const viewTitle = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("Pura")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("The UI Language for the Future.")]), PuraRuntime.elem('p')([])([PuraRuntime.text("Zero Runtime Errors. Zero Boilerplate.")])]); };
const viewProblem = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("The JS Nightmare")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("Why are we still debugging this?")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([PuraRuntime.text("const processUser = (user) => {\n  // 1. accidental assignment\n  if (user.role = 'admin') {\n    \n    // 2. defensive coding hell\n    if (user.data && \n        user.data.items && \n        user.data.items.length > 0) {\n          \n      return user.data.items.map(x => x * 2);\n    }\n  }\n  // Returns undefined implicitly...\n}")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("Undefined is not a function.")])]); };
const viewIrony = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("The Irony")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("I can hear you thinking...")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')(fragmentClass((idx >= 1)))])([PuraRuntime.text("\"You are bashing JavaScript...\"")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')(fragmentClass((idx >= 2)))])([PuraRuntime.text("\"But this slide is running in a Browser.\"")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')(fragmentClass((idx >= 3)))])([PuraRuntime.text("\"So you are using HTML/JS anyway.\"")])])]); };
const viewReveal = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("Actually... No.")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("This is Pura.")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("No manual HTML written.")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("No manual JavaScript written.")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("Compiled from Pura Source.")])])]); };
const viewTodoItem = function(item) { return PuraRuntime.elem('li')([PuraRuntime.attr('class')("todo-item")])([PuraRuntime.text(item)]); };
const viewDemo = function(model) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("Interactive Demo")]), PuraRuntime.elem('p')([])([PuraRuntime.text("A full state machine running inside the slide.")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("todo-container")])([PuraRuntime.elem('ul')([])(mapTodos(viewTodoItem)((list => list.slice(1))(model)))]), PuraRuntime.elem('div')([])([PuraRuntime.elem('button')([PuraRuntime.on('click')("ADD_TODO"), PuraRuntime.attr('class')("btn-primary")])([PuraRuntime.text("+ Add Task")]), PuraRuntime.elem('button')([PuraRuntime.on('click')("DEL_TODO"), PuraRuntime.attr('class')("btn-secondary")])([PuraRuntime.text("✓ Complete")])])]); };
const viewComparison = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("Syntax Comparison")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("two-column")])([PuraRuntime.elem('div')([])([PuraRuntime.elem('p')([])([PuraRuntime.text("JavaScript (Imperative)")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([PuraRuntime.text("const update = (msg, model) => {\n  switch (msg) {\n    case 'NEXT':\n      return {\n        ...model, \n        id: model.id + 1\n      };\n    default:\n      return model;\n  }\n};")])]), PuraRuntime.elem('div')([])([PuraRuntime.elem('p')([])([PuraRuntime.text("Pura (Expression)")]), PuraRuntime.elem('div')([PuraRuntime.attr('class')("code-block")])([PuraRuntime.text("let update = msg => model =>\n  if msg == \"NEXT\" then\n    { model | id = id + 1 }\n  else\n    model")])])])]); };
const viewInference = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("Inference Engine")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("The compiler is your pair programmer.")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("Hindley-Milner Type System")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("Catches errors before you save.")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("No Type Annotations required.")])])]); };
const viewRoadmap = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([])([PuraRuntime.text("Roadmap")]), PuraRuntime.elem('ul')([])([PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("Algebraic Effects (Koka style)")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("Native WebAssembly Backend")]), PuraRuntime.elem('li')([PuraRuntime.attr('class')("fragment visible")])([PuraRuntime.text("Time-Travel Debugger")])])]); };
const viewEnd = function(idx) { return slideWrapper([PuraRuntime.elem('h1')([PuraRuntime.attr('class')("title")])([PuraRuntime.text("Thank You")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("subtitle")])([PuraRuntime.text("Start writing better UI.")]), PuraRuntime.elem('p')([PuraRuntime.attr('class')("footer")])([PuraRuntime.text("Built with Pura Compiler 2025")])]); };
const view = function(model) { return (((idx) => (((idx === 0)) ? (viewTitle(idx)) : ((((idx === 1)) ? (viewProblem(idx)) : ((((idx === 2)) ? (viewIrony(3)) : ((((idx === 3)) ? (viewReveal(idx)) : ((((idx === 4)) ? (viewDemo(model)) : ((((idx === 5)) ? (viewComparison(idx)) : ((((idx === 6)) ? (viewInference(idx)) : ((((idx === 7)) ? (viewRoadmap(idx)) : (viewEnd(idx))))))))))))))))))(parseInt((list => list[0])(model)))); };
const main = (() => { return PuraRuntime.print("Starting Presentation (EN)..."); })();


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

  render();

  // --- KEYBOARD LISTENER ---
  document.addEventListener('keydown', (e) => {
    if (e.key === 'ArrowRight' || e.key === ' ') {
       dispatch("NEXT");
    }
    if (e.key === 'ArrowLeft') {
       dispatch("PREV");
    }
  });
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
