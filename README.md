# Kripke

Experimental repo to teach myself --- and, hopefully, other people --- about [Kripke semantics](https://en.wikipedia.org/wiki/Kripke_semantics) for various non-classical logics.

## Let me try it out!

There is a frontend [on gh-pages](http://www.harrisonrbrown.com/kripke/) but it should be considered highly unstable.

## Explanation

### Kripke models

Basic data types are found in the `Data.Kripke.Kripke` module. A `Node` represents a possible world; a `KripkeFrame` is a record containing an array `worlds` of `Node`s and an array `relation` of pairs of `Node`s; if `(w, x)` is an element of `relation` we say that `x` is _accessible_ from `w`.

An `Atom` is a variable; a `Model` is a record with a Kripke `frame`; an array `valuation` of pairs of `Atom`s and `Node`s representing the atoms that are true in a given world; and an array `domain` of pairs of `Atom`s and `Node`s.

An `Evaluation err expr` is a wrapper for a function `Model -> Either err (Node -> expr -> Boolean)`, that checks that a model is consistent with the axioms of a given logic and, if it is, evaluates whether a given expression is true in a given world. The `runEvaluation` unwrapper alters the type to `Model -> Node -> expr -> Either err Boolean`; the `checkModel` unwrapper alters it to `Model -> Either err Unit`.

### Model validation

For a `Model` to be a valid Kripke model for a given logic it typically must satisfy some axioms (depending on the logic), for instance the frame `relation` must be reflexive or transitive. Primitives for checking that a model conforms to certain rules are in `Data.Kripke.Validation`; these return applicatives whose error messages can be combined.

### Intuitionistic logics

[Intuitionistic logic](https://plato.stanford.edu/entries/logic-intuitionistic/) is a subset of classical logic that more accurately represents notions of constructive proof and of programming (especially typed functional programming). 

#### Intuitionistic propositional logic

[Propositional logic](https://en.wikipedia.org/wiki/Propositional_calculus) is the study of the truth or falsity of propositions formed by other propositions, such as "`a` and `b`", "`a` or `b`", or "`a` and `b` implies `b`". Intuitionistic propositional logic is the intuitionistic form of propositional logic.

An evaluation for intuitionistic propositional logic can be found in the `Logics.Intuitionistic.Propositional` module.

Much more to come here...

### Modal logics

[Modal logics](https://plato.stanford.edu/entries/logic-modal/) extend the expressions of classical logic to encompass various _modalities_ such as necessity, belief, obligation, and time.

#### Alethic logic

Alethic modal logic, often simply called "modal logic", deals with necessity and possibility. An evaluation for the propositional fragment of alethic modal logic can be found in the `Logics.Modal.Alethic.Propositional` module.