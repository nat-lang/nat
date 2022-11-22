## Nat v0.0.1

Nat is a language for modeling computation over typed domans, with applications in linguistic semantics and philosophical logic. Its syntax should be familiar to workers in either one of these fields, and to anyone familiar with functional programming. Nat has the following features:

### Basic Expressions

- Primitive literals and their common operations
- Variables
- Ternary conditionals
- Sets
- Trees
- Domains
- Quantifiers

### Lambda Abstraction & Application

At its heart, Nat is an implementation of the lambda calculus. This means variable binders may be introduced and eliminated. Lambda declarations look like this:

```
let id = \x. x
let succ = \x. x + 1
let Y = \f. (\x. f(x x)) (\x. f(x x))
```

And lambda applications like this:

```
id(1)
>>> 1

succ(1)
>>> 2

Y(\f.\n. if n <= 1 then 1 else n * f(n - 1))(5)
>>> 120
```

### Simply, Statically Typed

Nat has a monomorphic type system that rules out ill-formed terms. Its syntax is as follows, where `a` ranges over variables:

$$
type := <t> | <n> | <undef> | <\text{\_}> | <a> | <type, type>
$$

The boolean and integral domains have built in types, namely `<t>` and `<n>`, and built in operations over these domains are typed accordingly. You may specify additional type constraints as annotations on binders, like so:

```
let applySucc = \f:<n,n>.\x. f(x)

applSucc(succ)(True)
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

Types may be partially or completely omitted, and Nat's compiler will infer them via the hindley milner algorithm.

### Trees

Trees are first class citizens, with a dedicated syntax that desugars to a church encoding. Defining a new tree yields a catamorphism, which can in turn be used to perform arbitrary computations over the tree.

```
let tree = [0 [1][1]]

let accumulate = \data.\left.\right. data + left + right
let neutralData = 0

tree(neutralData)(accumulate)
>>> 2
```

### Quantification

The universal and existential quantifiers desugar to predication of their nuclear predicate against every element of their restricted scope, and always have type `<t>`.

```
dom N = {1, 2, 3, 4}

forall n in N. n < 5
>>> True

exists n in N. n > 5
>>> False
```
