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

module Inventory = struct
  type empty = Empty
  type 'a pickable = P
  type key = Key
end

module Previous = struct
  type left = Left
  type right = Right
  type down = Down
  type up = Up
  type none = None
end


module Events = struct
  type door = Door
  type stair = Stair
end

type border = <event:false'; free:false'; floor:Inventory.empty>
type free = <event:false'; free:true'; floor:Inventory.empty>
type key = <event:false'; free:true'; floor: Inventory.key Inventory.pickable>
type stair = <free:true';  event:Events.stair; floor:Inventory.empty>
type door = <event: Events.door; free:true'; stair:false'; floor: Inventory.key Inventory.pickable>


type f = free
type b = border

type won = Won


type 'a hzip = <
  l: 'a h;
  m: 'a;
  r: 'a h
> col

type ('a,'b) state =
  < world:'a; player:'b>



type player_start = < left_hand:Inventory.empty; right_hand:Inventory.empty; back:Previous.none >

module Builder = struct
  type _ elt =
    | F: free elt
    | W: border elt
    | S: stair elt
    | K: key elt
    | D: door elt
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

type 'a world = 'world constraint 'a = <world:'world; ..>
type 'a player = 'world constraint 'a = <world:'world; ..>


type 'a p = 'player
  constraint 'a = <player: 'player; ..>
type 'a w = 'w
  constraint 'a = <world: 'w; ..>


type ('a,'b, 'c) wmove = 'a -> <world: 'b; player: <left_hand:'lh; right_hand:'rh; back:'c> >
  constraint 'a = <world:'w; player: <left_hand:'lh; right_hand:'rh; back:'back> >

type 'a event = 'event
  constraint 'a = < event:'event; ..>

type ('a,'b,'c) eventless = ('a,'b,'c) wmove
    constraint
      'a w m m event = false'


type 'a left_hand = 'lh constraint 'a = <left_hand:'lh; ..>
type 'a right_hand = 'x constraint 'a = <right_hand:'x; ..>
type 'a fl = 'f constraint 'a = <floor:'f Inventory.pickable; ..>

type 'a st = 'f constraint 'a = <stair:'f; ..>



type 'a pick = <
  world:
    <l:'w l; r:'w r;
     m: <l:'m l;r:'m r;
         m:free
        >
    >;
  player:<left_hand:'m m fl; right_hand:'a p right_hand; back:Previous.none>
>
  constraint 'w = 'a w
  constraint 'm = 'w m


type 'a swap = <
  world: 'a world;
  player:<left_hand:'a p right_hand fl; right_hand:'a p left_hand; back:Previous.none>
>

type 'a open_door = <
  player:<left_hand:Inventory.empty; right_hand:'a p right_hand; back:Previous.none >;
  world:<
    l:'a world l;
    r: 'a world r;
    m: <l:'a world m l; m:free; r:'a world m r>;
  >
> constraint
  'a w m m event = Events.door
  constraint
  'a p left_hand = Inventory.key

type 'a move =
  | L: ('a, 'a world le, Previous.right ) eventless move
  | U: ('a, 'a world up, Previous.down ) eventless move
  | D: ('a, 'a world dw, Previous.up ) eventless move
  | R: ('a,'a world ri, Previous.left ) eventless move

  | P: ('a -> 'a pick) move
  | S: ('a -> 'a swap) move
  | O: ('a -> 'a open_door) move

  | Escape:  (<stair:true'; ..> m m world, won, Previous.none) wmove move


module type game = sig
  type start
  type _ path =
    | []: start path
    | (::): ('a -> 'b) move * 'a path -> 'b path
end


let game (type a b c d e f ma mb mc md me mf m mr r)
    (_: <l:a->b->c->d->e->f; m:<l:ma->mb->mc->md->me->mf;r:mr;m:m>; r: r> Builder.bcols):
  (module game
    with type start =
           <player:player_start;
            world:<l:a->b->c->d->e->f; m:<l:ma->mb->mc->md->me->mf;r:mr;m:m>; r: r>
                  >)
= (module struct
  type start = <player: player_start; world:<l:a->b->c->d->e->f; m:<l:ma ->mb->mc->md->me-> mf;r:mr;m:m>; r: r> >
  type _ path =
    | []: start path
    | (::): ('a->'b) move * 'a path -> 'b path
  end)

(*
module Level_test = (val game begin
    [ [K; F; F], F, [F; F; F] ;
      [F; F; F], F, [F; K; F] ;
      [F; F; F], F, [F; F; F] ],
    ( [F; F; F], F, [F; F; F] ),
    [ [F; K; F], F, [F; F; F] ;
      [F; F; F], F, [F; F; K] ;
      [F; F; F], F, [F; F; F] ]
  end)

module Test = struct
  open Level_test
  let _ =[]


  let _ = [R;L] = []

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
*)

module Level0 = struct
  module G= (val game begin
    [ [F; F; W], F, [F; F; F] ;
      [F; F; F], F, [W; W; F] ;
      [F; F; F], W, [W; W; F] ],
    ( [W; F; W], F, [K; D; F] ),
    [ [F; F; W], F, [W; W; W] ;
      [F; W; W], F, [F; F; W] ;
      [F; S; W], W, [W; W; W] ]
  end)
  open G
  let s = []
  let p = [D;O;D;P;D]
  let path = Escape::D::R::R::U::R::R::R::U::U::R::U::U::U::L::L::L::D::O::D::P::D::[]

end
