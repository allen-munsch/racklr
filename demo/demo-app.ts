function greet(name: string): string {
  return "Hello, " + name + "!";
}

let heading: string = greet("World");
let headingEl = document.createElement("h1");
headingEl.textContent = heading;
document.body.appendChild(headingEl);

let counter: number = 0;

let counterDiv = document.createElement("div");
counterDiv.id = "counter";
counterDiv.textContent = "Counter: " + counter;
document.body.appendChild(counterDiv);

let btn = document.createElement("button");
btn.textContent = "Click me";
btn.onclick = function(): void {
  counter = counter + 1;
  let el = document.getElementById("counter");
  if (el) {
    el.textContent = "Counter: " + counter;
  }
};
document.body.appendChild(btn);
