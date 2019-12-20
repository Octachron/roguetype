type  'a zip = 'a constraint
  'a = <l:'l; m:'m; r:'r>


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
  l: 'a -> 'a -> 'a -> 'a;
  m:'a;
  r:'a -> 'a -> 'a -> 'a
> col



type border = Border

type free = Free
type f = free
type b = border

type 'p cup =
  <l: 'f -> 'g -> 'h -> border;
   m:'e;
   r:'mid -> 'a -> 'b -> 'c >
  constraint
    'p = <
    l:'e -> 'f -> 'g -> 'h;
    m:'mid;
    r:'a -> 'b -> 'c -> 'd >

type 'p up = <
  l: 'a cup -> 'b cup -> 'c cup -> 'd cup;
  m: 'mid cup;
  r: 'e cup -> 'f cup -> 'g cup -> 'h cup
  > cols
  constraint 'p = <
    l: 'a col -> 'b col -> 'c col -> 'd col;
    m: (<l:free -> _; .. > as 'mid) col;
    r: 'e col -> 'f col -> 'g col -> 'h col
  > cols

type 'p le = <
  l: 'b -> 'c -> 'd -> border hzip ;
  m: 'a;
  r: 'mi -> 'e -> 'f -> 'g
  > cols
  constraint 'p = <
    l: (<m:free; ..> as 'a) -> 'b -> 'c -> 'd;
    m:'mi;
    r: 'e -> 'f -> 'g -> 'h
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

type _ path =
  | []: start path
  | (::): ('a,'b) move * 'a path -> 'b path

let s= []
let s = [U;U;L;L]
