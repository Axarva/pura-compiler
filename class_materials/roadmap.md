# Roadmap to 中間発表 (Sep 22) — Functional UI Compiler

## Timeline Overview

- Now – Aug 13: Active development
- Aug 14 – Aug 31: Vacation
- Sep 1 – Sep 21: Final push

## Final Goal for 9/22

Demonstrate a functional programming language that compiles to JavaScript, supports currying, partial application, and a basic MVP-style UI abstraction.

---

## Aug 6 – 13: Core Language + JS Codegen Prep

- [x] Finalize syntax design (done)
- [x] Decide target output: JavaScript (chosen)
- [x] Implement currying + partial application support in:
  - [x] Parser
  - [x] Type checker
  - [x] Evaluator
- [ ] Implement JS codegen for curried functions
- [ ] Write example programs to verify correctness
- [ ] Document design choices (for the report and 中間発表)

---

## Aug 14 – 31: Vacation Period

- [ ] Light brainstorming (if possible)
  - [ ] Think about MVP architecture (Model → View ← Update)
  - [ ] Sketch how effects (like ConsoleWrite / DOM updates) will work
- [ ] Keep a note of any ideas to implement in September

---

## Sep 1 – 21: MVP UI System + Final Demo Prep

- [ ] Implement basic Model-View-Update system:
  - [ ] Define model, update, view abstraction in the language
  - [ ] Generate JS glue code to render to DOM
  - [ ] Simple example: counter app or todo list
- [ ] Polish JS codegen
- [ ] Add REQUIRES DOMWrite-like effect annotation for DOM updates
- [ ] Write documentation and presentation slides
- [ ] Prepare demo code and backup screenshots

---

## Example MVP Target (for the demo)

```
model : Int
let model = 0

update : Msg -> Int -> Int
let update = msg => state =>
  match msg with
    | "INCR" => state + 1
    | "DECR" => state - 1
    | _      => state

view : Int -> Html
let view = state =>
  div [
    button("−", onClick("DECR")),
    text(state),
    button("+", onClick("INCR"))
  ]

main : Unit
let main = () => { runApp(model, update, view) } REQUIRES DOMWrite
```

## Notes
Goal is not a full React/Vue competitor — just demonstrate that our language can:

- Express interactive apps

- Compile to JavaScript

- Handle functional UI patterns
