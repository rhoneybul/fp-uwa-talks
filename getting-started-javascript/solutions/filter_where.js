/*
  Run this file with the command:
    "npx babel-node solutions/filter_where.js"

  Use < array.prototype.filter > to write a function 
  that takes a list of objects and finds all objects
  that match a given property.

  For Example:
    Input: 
      [ { id: 1, height: 20 },
        { id: 2, height: 25 },
        { id: 3, height: 20 } ], 
      { height: 20 }
    Output: 
      [ {id: 1, height: 20}, 
        {id: 3, height: 20} ]
*/

const ladders = [
  { id: 1, height: 20 },
  { id: 2, height: 25 },
  { id: 3, height: 20 },
  { id: 4, height: 25 },
  { id: 5, width: 15 }
];

let result;

// ES6+ Style

const filterWhere = (array, searchItem) => {
  const key = Object.keys(searchItem)[0];
  const value = searchItem[key];
  return array.filter(({ [key]: foo }) => foo === value);
};

result = filterWhere(ladders, { height: 25 });
console.log("ES6+ Style:", result);

// with Currying

const hasValue = key => value => object => object[key] === value;

result = ladders.filter(hasValue("height")(25));
console.log("Curried (1 Step):", result);

const hasHeight = hasValue("height");
result = ladders.filter(hasHeight(25));
console.log("Curried (2 Steps):", result);

// ES5 Style

function filterWhere_old(array, searchItem) {
  var key = Object.keys(searchItem)[0];
  var value = searchItem[key];
  return array.filter(function(element) {
    return element[key] === value || false;
  });
}

result = filterWhere_old(ladders, { height: 25 });
console.log("ES5 Style:", result);
