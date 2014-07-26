open Gcc_types

let scope ~name ~var expr = Call (Fn ([name], expr), [var])
