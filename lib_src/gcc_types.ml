open Core_kernel.Std

module Address = Unique_id.Int(Unit)

type address = Address.t

type instruction =
  | LDC of int               (* load constant *)
  | LD of int * int          (* load from environment *)
  | ADD          (* integer addition *)
  | SUB          (* integer subtraction *)
  | MUL          (* integer multiplication *)
  | DIV          (* integer division *)
  | CEQ          (* compare equal *)
  | CGT          (* compare grate than *)
  | CGTE         (* compare greater than or equal *)
  | ATOM         (* test if value is an integer *)
  | CONS         (* allocate a CONS cell *)
  | CAR          (* first *)
  | CDR          (* second *)
  | SEL of address * address         (* conditional branch *)
  | JOIN         (* return from branch *)
  | LDF of address          (* load function *)
  | AP of int          (* call function *)
  | RTN           (* return from function call *)
  | DUM of int         (* create empty env frame *)
  | RAP of int         (* recursive environment call function *)
  | STOP         (* terminate co-processor execution *)
  | TSEL of address * address         (* tail-call conditional branch *)
  | TAP  of int        (* tail-call function *)
  | TRAP of int        (* recursive env tail-call function *)
  | ST of int * int          (* store to env *)
  | DBG          (* printf debugging *)
  | BRK          (* breakpoint debugging *)
  | LABEL of address
  | COMMENT of string

let is_phony = function
  | LABEL _ | COMMENT _ -> true
  | _ -> false

type var = string with sexp

type expr =
  | Const of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Cons of expr * expr
  | Fn of var list * expr
  | Call of expr * expr list
  | If of expr * expr * expr
  | Eq of expr * expr
  | Gt of expr * expr
  | Gte of expr * expr
  | Var of var
  | Car of expr
  | Cdr of expr
with sexp