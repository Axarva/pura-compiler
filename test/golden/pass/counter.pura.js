const PuraRuntime = {
  elem: (tag) => (attrs) => (children) => ({ tag, attrs, children, key: null }),
  text: (str) => ({ tag: 'TEXT_NODE', text: String(str) }),
  on: (eventName) => (msg) => ({ type: 'event', name: eventName, msg: msg }),
  attr: (name) => (val) => ({ type: 'attribute', name: name, value: val }),
  print: (str) => console.log(str)
};


const initialModel = 0;
const update = function(msg) { return function(model) { return (() => { return (((msg === "0")) ? ((() => { return (model + 1); })()) : ((() => { return (((msg === "1")) ? ((() => { return (model - 1); })()) : (model)); })())); })(); }; };
const view = function(model) { return (() => { return PuraRuntime.elem('div')([])([PuraRuntime.elem('h1')([])([PuraRuntime.text("Pura Counter")]), PuraRuntime.elem('p')([])([PuraRuntime.text(("Count: " + String(model)))]), PuraRuntime.elem('button')([PuraRuntime.on('click')("0")])([PuraRuntime.text("+")]), PuraRuntime.elem('button')([PuraRuntime.on('click')("1")])([PuraRuntime.text("-")])]); })(); };


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
