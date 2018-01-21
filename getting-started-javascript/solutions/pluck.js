/*
  Run this file with the command:
    "npx babel-node solutions/pluck.js"

  Using < array.prototype.map > write a function
  that accepts an array and a string representing 
  a property name and return an array containing 
  that property from each object. 
  
  For Example:
    Input: [{color: "red"}, {color: "blue"}], "color"
    Output: ["red", "blue"]
*/

const paints = [{ color: "red" }, { color: "blue" }];

let result;

// ES6+ Style

const pluck = (array, property) =>
  array.map(({ [property]: plucked }) => plucked);

result = pluck(paints, "color");
console.log("ES6+ Style:", result);

// With Currying

const pluck_curry = property => array =>
  array.map(({ [property]: plucked }) => plucked);

result = pluck_curry("color")(paints);
console.log("Curried (1 Step):", result);

const pluck_color = pluck_curry("color");
result = pluck_color(paints);
console.log("Curried (2 Steps):", result);

// ES5 Style

function pluck_old(array, property) {
  return array.map(function(element) {
    return element[property];
  });
}

result = pluck_old(paints, "color");
console.log("ES5 Style:", result);
