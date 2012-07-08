TODO List
=========

Part One
--------

* Nothing; I don't want to scare the reader.

Part Two
--------

Programming exercises:

* Redefine the value field of the `Chain` type using type aliases. This is a
  purely aesthetic improvement, used to provide the programmer with a better
  understanding of the code.
* Redefine the `Chain` type using the `newtype` Haskell keyword, which is
  definitely more suitable for the task at hand. Modify the rest of the code to
  match the new definition.
* Redefine the `accessibleStates` function using the [`maybe`][1] function.
* Redefine `next'` and `randomWalk` using [guards][2] instead of if-then-else.
* Reimplement `next` using another sampling scheme. Explain how this affects
  the distribution of accessible states if that's the case.

Theory exercises/questions:

* Add another state, "Foggy", to the weather example. Define the probabilities
  at will, checking however that they are properly normalized.
* Add another variable, "Wind" to the weather Markov chain. Think about
  correlations between wind and sun/weather and choose probabilities
  accordingly. Choose possible states for the new variable: `Calm`, `Windy`,
  `VeryWindy` and whatever else feels right.
* Experiment with the above examples and draw some preliminary conclusions.

[1]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:maybe
[2]: http://en.wikibooks.org/wiki/Haskell/Control_structures#if_and_guards_revisited
