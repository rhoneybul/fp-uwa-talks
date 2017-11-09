/*
  Run this file with the command:
    "npx babel-node problems/batsmen.js"

  Read in batsmen from a comma separated file and
  convert them into a list of objects with 
  initials, surnames, runs, and averages.
  
  Round the averages to the nearest integer, sort the batsmen 
  in descending order by total runs and filter to only 
  include batsmen with a surname that starts with C.

  For Example:
    Input:  AN Cook, 11629, 46.33, 1 \n
            GA Gooch, 8900, 42.58, 2 \n
            DI Gower, 8231, 44.25, 3 \n
    Output: 
      [ {inits: AN, surname: Cook, tot: 11629, avg: 46} ]
*/

import fs from "fs";

const output = fs
  .readFileSync("assets/batsmen-data.txt", "utf8")
  .trim() // trims whitespace
  .split("\n") // splits file into lines
  .map(text => text); //chain your methods here like so

// your time to shine.

console.log(output);
