type 'p col = 'p constraint
  'p = <
    up:'a -> 'b -> 'c -> 'd;
    mi:'mid;
    dw: 'e -> 'f -> 'g -> 'h
  >

type 'p cols = 'p constraint
  'p = <
    le:'a col -> 'b col -> 'c col -> 'd col;
    mi:'mid col;
    ri: 'e col -> 'f col -> 'g col -> 'h col
  >


type 'a hcol = <
  up: 'a -> 'a -> 'a -> 'a;
  mi:'a;
  dw:'a -> 'a -> 'a -> 'a
> col



type border = Border

type free = Free
type f = free
type b = border

type 'p cup =
  <up: 'f -> 'g -> 'h -> border;
   mi:'e;
   dw:'mid -> 'a -> 'b -> 'c >
  constraint
    'p = <
    up:'e -> 'f -> 'g -> 'h;
    mi:'mid;
    dw:'a -> 'b -> 'c -> 'd >

type 'p up = <
  le: 'a cup -> 'b cup -> 'c cup -> 'd cup;
  mi: 'mid cup;
  ri: 'e cup -> 'f cup -> 'g cup -> 'h cup
  > cols
  constraint 'p = <
    le: 'a col -> 'b col -> 'c col -> 'd col;
    mi: (<up:free -> _; .. > as 'mid) col;
    ri: 'e col -> 'f col -> 'g col -> 'h col
  > cols

type 'p le = <
  le: 'b -> 'c -> 'd -> border hcol ;
  mi: 'a;
  ri: 'mi -> 'e -> 'f -> 'g
  > cols
  constraint 'p = <
    le: (<mi:free; ..> as 'a) -> 'b -> 'c -> 'd;
    mi:'mi;
    ri: 'e -> 'f -> 'g -> 'h
 > cols


type ('f,'b) line = 'f -> 'f -> 'b -> 'b

type ('a,'b) scol = < up: 'a ;mi:'b; dw:'a> col

type ('a,'b) scols = < le: 'a ;mi:'b; ri:'a> cols

type mcol = ((f,b) line, f) scol

type start = (
  (mcol, b hcol) line,
  mcol
) scols

type ('a,'b) move =
  | L: ('a, 'a le) move
  | U: ('a, 'a up) move

type _ path =
  | []: start path
  | (::): ('a,'b) move * 'a path -> 'b path

let s= []
let s = [U;U;L;L]
