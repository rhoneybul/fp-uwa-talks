import fs from "fs";

const output = fs
  .readFileSync("../assets/batsmen-data.txt", "utf8")
  .trim()
  .split("\n")
  .map(line =>
    line
      .split(",")
      .map(val => val.trim())
      .map(val => parseInt(val) || val)
  )
  .map(([name, ...rest]) => [...name.split(" "), ...rest])
  .map(([inits, surname, runs, average]) => ({ inits, surname, runs, average }))
  .sort((a, b) => b.runs - a.runs) //sort descending by runs
  .filter(({ surname }) => surname.startsWith("C")); // batter starts with M

console.log(output);
