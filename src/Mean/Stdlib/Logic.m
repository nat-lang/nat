let true = \x\y . x
let false = \x\y . y

let if = \b\t\f . b(t)(f)

let and = \x\y . if(x)(y)(false)
let or = \x\y . if(x)(true)(y)

let not = \x\y . if(x)(true)(false)

let 0' = \f\x . x
let 1' = \f\x . f x
let 2' = \f\x . f(f x)
let 3' = \f\x . f(f(f x))

let succ = \n\f\x . f(n f x)

let add = \m\n . m succ n
let add' = \m\n\f\x . m f (n f x)

let mul = \m\n . m(add n)0'
let exp = \m\n . m(mul n)1'