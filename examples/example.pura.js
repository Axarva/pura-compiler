const print = console.log;
const toString = (x) => x.toString();

const greet = function(name) { return function(name2) { return (() => { return (((("Hello, " + name) + " and ") + name2) + "!"); })(); }; };
const greetAsa = greet("Asa");
const x = "10";
const addOne = (a => b => a + b)(1);
const y = x;

// --- Execute Main ---
addOne(5);
