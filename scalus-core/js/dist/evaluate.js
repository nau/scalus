const {Scalus} = require( "./scalus-opt-bundle.js");
const result = Scalus.evaluateScript("450101004981");
console.log(result);
console.log(result.budget.memory);
