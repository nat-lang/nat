# Nat v0.0.1

Nat is a language for modeling computation over typed domains, with applications in linguistic semantics and philosophical logic. Its syntax should be familiar to those with a background in either one of these fields, or in functional programming.

## Introduction

### Basic Expressions

- [x] Primitive literals and their built in operations
- [x] Variables
- [x] Sets
- [x] Trees
- [x] Domains
- [x] Quantifiers
- [x] Ternary conditionals
- [x] Case statements
  - [x] Literal cases
  - [x] Type cases
  - [x] Pattern matching on types
  - [x] Pattern matching on expressions
  - [x] Wildcards
- [x] First-class binders
- [ ] Set comprehensions
- [ ] Infinite domains
- [ ] Recursive domains
- [ ] Bounded domains
- [ ] Modules

### Lambda Abstraction & Application

At its heart, Nat is an implementation of the lambda calculus. This means that you may define functions and apply them to arguments. Functions are named via `let` declarations or left anonymous.

```
let id = \x. x
let succ = \x. x + 1
```

Function application is simply left associative adjacency. Parentheses serve only to disambiguate association.

```
id 1
>>> 1

succ 1
>>> 2

succ (succ 1)
>>> 3
```

Functions may reference themselves in their own bodies.

```
\\ explicit recursion with the y combinator
let Y = \f. (\x. f(x x)) (\x. f(x x))
let fact = Y(\f.\n. if n <= 1 then 1 else n * f(n - 1))

\\ implicit recursion with the y combinator
let fact' = \n. if n <= 1 then 1 else n * fact'(n - 1)

fact 5
>>> 120

fact' 5
>>> 120
```

### Simply, Statically Typed

Nat has a monomorphic type system that rules out ill-formed terms at compile-time. Its syntax is as follows, where `a` ranges over variables:

```math
type := \langle t \rangle
        \enspace | \enspace
        \langle n \rangle
        \enspace | \enspace
        \langle undef \rangle
        \enspace | \enspace
        \langle \_ \rangle \enspace
        \enspace | \enspace
        \langle a \rangle \enspace
        \enspace | \enspace
        \langle type,type \rangle
```

The boolean and integral domains have built in types, namely `<t>` and `<n>`, and built in operations over these domains are typed accordingly. You may specify additional type constraints as annotations on binders, like so:

```
let applySucc = \f:<n,n>.\x. f(x)

applySucc succ True
>>> UnificationError (NotUnifiable <t> <n>)
```

### Typed Domains

Type constructors come in the form of domain declarations.

```
dom W = {w1, w2, w3}

:t w1
>>> <W>
```

### Inference

Types may be partially or completely omitted and Nat will infer them.

### Trees

Trees are first class citizens, with a dedicated syntax that desugars to a church encoding. Defining a new tree yields a fold, which can in turn be used to perform arbitrary operations on the tree.

```
let tree = [0 [1][1]]

let accumulate = \data.\left.\right. data + left + right
let neutralData = 0

tree(neutralData)(accumulate)
>>> 2
```

### Quantification

The universal and existential quantifiers desugar to application of their nuclear predicate to every element of their restricted scope, and always have type `<t>`.

```
dom N = {1, 2, 3, 4}

forall n in N. n < 5
>>> True

exists n in N. n > 5
>>> False
```

The iota quantifier finds the unique member of the restricted scope that satisfies its nuclear predicate, and otherwise returns `undefined`. Its type, for any domain `X`, is `X ⋃ undefined`.

```
the n in N. n == 4
>>> 4

the n in N. n > 4
>>> undefined
```

## Modeling Linguistic Semantics

### Composition Operations

**Type-driven function application** over a tree may be modeled with the `tycase` expression.

```
let succ = \x. x + 1

// define a tree with both left- and right-branching
// function nodes
let tree = [undef [undef [succ] [0]] [succ]]

// pattern match on the types of the nodes, applying
// functions to their arguments
let TDFA = \l.\r. tycase (l, r) of
    (l',r'):(<A>, <A,B>) -> r' l'
  | (l',r'):(<A,B>, <A>) -> l' r'
  | _ -> undef

// compute only the values of nodes that are undefined
let guard = \op.\x.\l.\r. tycase x of
    undef:<undef> -> op l r
  | _ -> x

// put it all together
let compose = \tree.\op. tree undef (guard op)

// execute it
compose tree TDFA
>>> 2
```

**Type driven predicate coordination** proceeds similarly.

```
// define a tree with two children of the same type
let tree = [undef [True] [False]]

// pattern match on the types of the children, conjoining
// children of the same type
let TDPC = \l.\r. tycase (l, r) of
    (l',r'):(<A>, <A>) -> l' && r'
  | _ -> undef

compose tree TDPC
>>> False
```

**Type driven predicate abstraction** requires a construct that will be unfamiliar to anyone with a background purely in computer science. In Nat, binders have a semantics of their own: to introduce a solitary binder over the variable `x`, write `\x`. To eliminate it, apply it, as if it were a function, to an arbitrary expression `e`. This will produce a function that binds any free occurences of `x` in `e`. As a practical matter, Nat only allows first-class binders as the expression nodes of trees, but since trees reduce to lambdas under the hood, binders are fully integrated into Nat's semantics.

```
// a tree with a binding child and a free variable child
let tree = [undef [\x] [x]]

// pattern match on the type of a binder
let TDPA = \l.\r. tycase (l, r) of
   (l',r'):(<A,>, <B>) -> l' r'
  | _ -> undef

// using our previously defined utility functions
compose tree TDPA
>>> \x. x
```

### Generalized Quantifiers

We have functions and quantifiers, so generalizing quantifiers is straightforward.

```
let N = {0, 1, 2, 3, 4, 5}

let every = \d.\q.\p. forall e in d. (q e) => (p e)
let some = \d.\q.\p. exists e in d. (q e) => (p e)

let lessThan2 = \x. x < 2
let lessThan5 = \x. x < 5

every N lessThan2 lessThan5
>>> True

some N lessThan2 lessThan5
>>> True

every N lessThan5 lessThan2
>>> False

some N lessThan5 lessThan2
>>> True
```

### Structuring Domains

#### mereology

#### mass & plurality

#### intensionality

### Context Change
