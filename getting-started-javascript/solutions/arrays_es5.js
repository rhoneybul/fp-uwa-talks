// ES5 Array Functions

// Map

var paints = [{ color: "red" }, { color: "blue" }];

function pluck(array, property) {
  return array.map(function(element) {
    return element[property];
  });
}

console.log(pluck(paints, "color"));

// Reduce

var numbers = [1, 1, 2, 3, 4, 4];

function unique(array) {
  return array.reduce(function(prev, curr) {
    if (!prev.includes(curr)) {
      prev.push(curr);
    }
    return prev;
  }, []);
}

console.log(unique(numbers));

// Filter

var ladders = [
  { id: 1, height: 20 },
  { id: 2, height: 25 },
  { id: 3, height: 20 },
  { id: 4, height: 25 }
];

function filterWhere(array, searchItem) {
  var key = Object.keys(searchItem)[0];
  var value = searchItem[key];
  return array.filter(function(element) {
    return element[key] === value || false;
  });
}

console.log(filterWhere(ladders, { height: 25 }));
