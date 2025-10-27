const PuraRuntime = {
  elem: (tag) => (attrs) => (children) => ({ tag, attrs, children, key: null }),
  text: (str) => ({ tag: 'TEXT_NODE', text: String(str) }),
  on: (eventName) => (msg) => ({ type: 'event', name: eventName, msg: msg }),
  print: (str) => console.log(str)
};


const makeIndent = function(n) { return (((n <= 0)) ? ("") : (("  " + makeIndent((n - 1))))); };
const printIndented = function(indent) { return function(str) { return PuraRuntime.print((makeIndent(indent) + str)); }; };
const drawTree = function(size) { return function(indent) { return (((size > 0)) ? ((() => { printIndented(indent)("*");drawTree((size - 1))((indent + 1));return drawTree((size - 1))((indent + 1)); })()) : ((() => { return null; })())); }; };
const main = (() => { PuraRuntime.print("--- Generating Recursive Tree ---");drawTree(4)(0);return PuraRuntime.print("-------------------------------"); })();


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
      }
      // Add other attribute types here (e.g., className)
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
