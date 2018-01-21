/*
  Run this file with the command:
    "npx babel-node problems/make_tree.js"

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

// Your time to shine.

console.log(JSON.stringify(result));
