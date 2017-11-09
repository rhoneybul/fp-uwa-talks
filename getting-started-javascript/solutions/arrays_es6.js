// ES6 Array Functions

// Map

const paints = [{ color: "red" }, { color: "blue" }];

const pluck = (array, property) =>
  array.map(({ [property]: plucked }) => plucked);

console.log(pluck(paints, "color"));

// Reduce

const numbers = [1, 1, 2, 3, 4, 4];

const unique = array =>
  array.reduce((prev, curr) => {
    if (!prev.includes(curr)) {
      prev.push(curr);
    }
    return prev;
  }, []);

console.log(unique(numbers));

// Filter

const ladders = [
  { id: 1, height: 20 },
  { id: 2, height: 25 },
  { id: 3, height: 20 },
  { id: 4, height: 25 }
];

const filterWhere = (array, searchItem) => {
  const key = Object.keys(searchItem)[0];
  const value = searchItem[key];
  return array.filter(({ [key]: foo }) => foo === value);
};

console.log(filterWhere(ladders, { height: 25 }));
