type combine('result, 'element) = ('result, 'element) => 'result;

type projection('a, 'b) = 'a => 'b;
let map: projection('original, 'target) => combine('result, 'target) => ('result, 'original) => 'result =
  projection => combine => (result, element) => combine(result, projection(element));

type predicate('a) = 'a => bool;
let filter: predicate('element) => combine('result, 'element) => ('result, 'element) => 'result =
  predicate => combine => (result, element) =>
    if (predicate(element)) {
      combine(result, element);
    } else {
      result;
    };
