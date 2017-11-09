/*
  Run this file with the command:
    "npx babel-node solutions/make_tree.js"

  Using array functions and recursion write a function
  that take a list of objects representing a flattened 
  heirarchy and build it into a tree by connecting on id 
  and parent properties. Hint: < array.prototype.filter >
  and < array.prototype.forEach > might be helpful.
  
  For Example:
    Input: 
      [
        { id: "animals", parent: null },
        { id: "mammals", parent: "animals" },
        { id: "cats", parent: "mammals" },
        { id: "dogs", parent: "mammals" },
        { id: "labrador", parent: "dogs" },
        { id: "persian", parent: "cats" } ]
    Output: {
      "animals":{
        "mammals":{
          "cats":{ "persian":{} },
          "dogs":{ "labrador":{}, etc ...
*/

const animals = [
  { id: "animals", parent: null },
  { id: "mammals", parent: "animals" },
  { id: "cats", parent: "mammals" },
  { id: "dogs", parent: "mammals" },
  { id: "labrador", parent: "dogs" },
  { id: "poodle", parent: "dogs" },
  { id: "persian", parent: "cats" }
];

let result;

// ES6+ Style

const makeTree = (tree, parent = null, node = {}) => {
  tree
    .filter(c => c.parent === parent)
    .forEach(c => (node[c.id] = makeTree(tree, c.id)));
  return node;
};

result = makeTree(animals);
console.log("ES6+ Style:", JSON.stringify(result));

// With Currying

const makeTree_curry = parent => tree => {
  const node = {};
  tree
    .filter(c => c.parent === parent)
    .forEach(c => (node[c.id] = makeTree_curry(c.id)(tree)));
  return node;
};

result = makeTree_curry(null)(animals);
console.log("Curried (1 Step):", JSON.stringify(result));

const makeMammalTree = makeTree_curry("mammals");
result = makeMammalTree(animals);
console.log("Curried (2 Steps):", JSON.stringify(result));

// ES5 Style

function makeTree_old(tree, parent) {
  parent = typeof parent !== "undefined" ? parent : null;
  var node = {};
  tree
    .filter(function(c) {
      return c.parent === parent;
    })
    .forEach(function(c) {
      return (node[c.id] = makeTree_old(tree, c.id));
    });
  return node;
}

result = makeTree_old(animals);
console.log("ES5 Style:", JSON.stringify(result));
