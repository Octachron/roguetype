type  'a zip = 'a constraint
  'a = <l:'l; m:'m; r:'r>

type 'a h = 'a -> 'a -> 'a -> 'a -> 'a -> 'a

type 'p col = 'p constraint
  'p = <
    l:'a -> 'b -> 'c -> 'd -> 'e -> 'f;
    m:'mid;
    r: 'r1 -> 'r2 -> 'r3 -> 'r4 -> 'r5 ->'r6
  > zip

type 'p cols = 'p constraint
  'p = <
    l:'l1 col -> 'l2 col -> 'l3 col -> 'l4 col -> 'l5 col -> 'l6 col;
    m:'mid col;
    r: 'e col -> 'f col -> 'g col -> 'h col -> 'r5 col -> 'r6 col
  > zip

type true'
type false'

type border = <free:false'; stair: false'>
type free = <free:true'; stair: false'>
type f = free
type b = border

type won = Won
type stair = <free:true'; stair:true'>


type 'a hzip = <
  l: 'a h;
  m: 'a;
  r: 'a h
> col

module Builder = struct
  type _ elt =
    | F: free elt
    | W: border elt
    | S: stair elt
  type 'a l =
    | []: (border -> border -> border) l
    | (::): 'a elt * 'b l -> ('a -> 'b) l

  type 'p inv = 'c -> ' b -> 'a -> 'e -> 'f -> 'g
    constraint 'p = 'a -> 'b -> 'c -> 'e -> 'f -> 'g

  type 'p bcol = 'l inv l * 'm elt * 'r l
    constraint 'p = <l:'l; m:'m; r:'r >



  type 'a ll =
    | []: (border hzip -> border hzip -> border hzip) ll
    | (::): 'p bcol * 'b ll -> ('p -> 'b) ll

  type 'p bcols = 'l inv ll * 'm bcol * 'r ll
    constraint 'p = <l:'l; m:'m; r:'r >

end



type 'p first = 'a
  constraint 'p = 'a -> 'b -> 'c -> 'd -> 'e -> 'f

type ('p,'x) app = 'a -> 'b -> 'c -> 'd-> 'e -> 'x
  constraint 'p = 'a -> 'b -> 'c -> 'd -> 'e -> 'f


type ('p,'x) pull = 'b -> 'c -> 'd -> 'e -> 'f -> 'x
  constraint 'p = 'a -> 'b -> 'c -> 'd -> 'e -> 'f

type ('x,'p) push = 'x -> 'a -> 'b -> 'c -> 'd -> 'e
  constraint 'p = 'a -> 'b -> 'c -> 'd -> 'e -> 'f



type 'a is_free = 'a constraint 'a = <free:true'; ..>
type 'a mfree = < m: <free:true'; ..> ; ..> as 'a

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

type 'p lcup = 'a cup -> 'b cup -> 'c cup -> 'd cup -> 'e cup -> 'f cup
    constraint 'p = 'a -> 'b -> 'c -> 'd -> 'e -> 'f

type 'p lcdw = 'a cdw -> 'b cdw -> 'c cdw -> 'd cdw  -> 'e cdw -> 'f cdw
    constraint 'p = 'a -> 'b -> 'c -> 'd -> 'e -> 'f



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

type ('f,'b) line = 'f -> 'f -> 'f -> 'b -> 'b -> 'b

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
  | Escape: (<m: <m:<stair:true'; ..> ; .. >; .. >, won) move


module type game = sig
  type start
  type _ path =
    | []: start path
    | (::): ('a,'b) move * 'a path -> 'b path
end


let game (type a b c d e f g ma mb mc md me mf l m mr r)
    (x: <l:a->b->c->d->e->f; m:<l:ma->mb->mc->md->me->mf;r:mr;m:m>; r: r> Builder.bcols):
  (module game with type start = <l:a->b->c->d->e->f; m:<l:ma->mb->mc->md->me->mf;r:mr;m:m>; r: r> )
= (module struct
  type start = <l:a->b->c->d->e->f; m:<l:ma ->mb->mc->md->me-> mf;r:mr;m:m>; r: r>
  type _ path =
    | []: start path
    | (::): ('a,'b) move * 'a path -> 'b path
  end)

module Level_test = (val game begin
    [ [F; F; F], F, [F; F; F] ;
      [F; F; F], F, [F; F; F] ;
      [F; F; F], F, [F; F; F] ],
    ( [F; F; F], F, [F; F; F] ),
    [ [F; F; F], F, [F; F; F] ;
      [F; F; F], F, [F; F; F] ;
      [F; F; F], F, [F; F; F] ]
  end)

module Test = struct
  open Level_test
  let s=[]
  let s = [R;L] = []
  let s = [U;D] = []

  let t = [L;L;L;D;R;R;R] = [D]
  let w = [L;L;L;L;D;D;D;R;R;R]

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
end


module Level0 = struct
  module G= (val game begin
    [ [F; F; W], F, [F; F; F] ;
      [F; F; F], F, [W; W; F] ;
      [F; F; F], W, [W; W; F] ],
    ( [W; F; W], F, [F; F; F] ),
    [ [F; F; W], F, [W; W; W] ;
      [F; W; W], F, [F; F; W] ;
      [F; S; W], W, [W; W; W] ]
  end)
  open G
  let s = []
  let path = Escape::D::R::R::U::R::R::R::U::U::R::U::U::U::L::L::L::D::D::D::[]

end
