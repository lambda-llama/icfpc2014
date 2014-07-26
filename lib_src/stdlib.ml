open Types

let pair a b = Cons (Const a, Const b)


let scope name var expr = Call (Fn ([name], expr), [var])
