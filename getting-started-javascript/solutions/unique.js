/*
  Run this file with the command:
    "npx babel-node solutions/unique.js"

  Using < array.prototype.reduce > write a function 
  called 'unique' that will remove all the duplicate 
  values from an array.

  For Example:
    Input: [1, 1, 2, 3, 4, 4]
    Output: [1, 2, 3, 4]
*/

const numbers = [1, 1, 2, 3, 4, 4];

// ES6+ Style

const unique = array =>
  array.reduce((prev, curr) => {
    if (!prev.includes(curr)) {
      prev.push(curr);
    }
    return prev;
  }, []);

console.log("ES6+ Style:", unique(numbers));

// ES5 Style

function unique_old(array) {
  return array.reduce(function(prev, curr) {
    if (!prev.includes(curr)) {
      prev.push(curr);
    }
    return prev;
  }, []);
}

console.log("ES5 Style:", unique_old(numbers));
