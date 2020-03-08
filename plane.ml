type  'a zip = 'a constraint
  'a = <l:'l; m:'m; r:'r>

type 'a h = 'a -> 'a -> 'a -> 'a

type 'p col = 'p constraint
  'p = <
    l:'a -> 'b -> 'c -> 'd;
    m:'mid;
    r: 'e -> 'f -> 'g -> 'h
  > zip

type 'p cols = 'p constraint
  'p = <
    l:'a col -> 'b col -> 'c col -> 'd col;
    m:'mid col;
    r: 'e col -> 'f col -> 'g col -> 'h col
  > zip


type 'a hzip = <
  l: 'a h;
  m: 'a;
  r: 'a h
> col


type 'p first = 'a
  constraint 'p = 'a -> 'b -> 'c -> 'd

type ('p,'x) app = 'a -> 'b -> 'c -> 'x
  constraint 'p = 'a -> 'b -> 'c


type ('p,'x) pull = 'b -> 'c -> 'd -> 'x
  constraint 'p = 'a -> 'b -> 'c -> 'd

type ('x,'p) push = 'x -> 'a -> 'b -> 'c
  constraint 'p = 'a -> 'b -> 'c -> 'd

type border = Border

type free = Free
type f = free
type b = border


type 'a is_free = 'a constraint 'a = free
type 'a mfree = < m: free; ..> as 'a

type 'p l = 'l
  constraint 'p = <l:'l; .. >

type 'p r = 'r
  constraint 'p = <r:'r; .. >

type 'p m = 'm
  constraint 'p = <m:'m; .. >


type 'p cup = <
  l: ('p l,b) pull;
  m:'p l first;
  r: ('p m, 'p r) push
>

type 'p cdw =
  <l: ('p m, 'p l) push;
   m:'p r first;
   r: ('p r, b) pull >

type 'p lcup = 'a cup -> 'b cup -> 'c cup -> 'd cup
    constraint 'p = 'a -> 'b -> 'c -> 'd

type 'p lcdw = 'a cdw -> 'b cdw -> 'c cdw -> 'd cdw
    constraint 'p = 'a -> 'b -> 'c -> 'd



type 'p up = <
  l: 'p l lcup;
  m: 'p m cup mfree;
  r: 'p r lcup
  > cols

type 'p dw = <
  l: 'p l lcdw;
  m: 'p m cdw mfree;
  r: 'p r lcdw
  > cols

type 'p le = <
  l: ('p l, b hzip) pull;
  m: 'p l first mfree;
  r: ('p m, 'p r) push
  > cols

type 'p ri = <
  l: ('p m, 'p l) push;
  m: 'p r first mfree;
  r: ('p r, b hzip) pull
  > cols

type ('f,'b) line = 'f -> 'f -> 'b -> 'b

type ('a,'b) scol = < l: 'a ;m:'b; r:'a> col


type mcol = ((f,b) line, f) scol

type start = (
  (mcol, b hzip) line,
  mcol
) scol

type ('a,'b) move =
  | L: ('a, 'a le) move
  | U: ('a, 'a up) move
  | D: ('a, 'a dw) move
  | R: ('a,'a ri) move

type _ path =
  | []: start path
  | (::): ('a,'b) move * 'a path -> 'b path

let s=[]
let s = [R;L] = []
let s = [U;D] = []

let s = [U;L] = [L;U]
let s = [D;L] = [L;D]
let s = [U;R] = [R;U]
let s = [D;R] = [R;D]


let hamiltonian (x: start path) = match x with
  | [] -> ()
  | [_] -> .
  | [_;_] -> ()
  | [_;_;_] -> .
  | _ -> ()
