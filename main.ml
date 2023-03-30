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

type yes = True
type no = False

module Previous = struct
  type left = Left
  type right = Right
  type down = Down
  type up = Up
  type none = None
end

module Inventory = struct
  type none = None
  type key = Key
  type dagger = Dagger
  type sword = Sword
  type mithril_sword = Mithril_sword
  type axe = Axe
  type pickaxe = Axe
  type greatsword = Great_sword
  type 'a arrow = Arrow of 'a
  type minor_health_potion = Minor_health_potion
  type major_health_potion = Minor_health_potion
  type stamina_potion = Stamina_potion
  type regeneration_potion = Regeneration_potion
  type ring_of_fire = Ring_of_fire
  type ring_of_water = Ring_of_water
end

module Obstacle = struct
  type tree = Tree
  type rock = Rock
  type fire = Fire
end


module Integer_range = struct

  type token = Token
  type none = None
  module Modifier = struct
    type 'a one = token -> 'a
    type 'a two = 'a one one
    type 'a three = 'a one two
    type 'a four = 'a two two
    type 'a eight = 'a four four
  end


  type 'a start = <current:'a; potential: none>

  type one = (token -> none) start
  type two = (token -> token -> none) start
  type three = (token -> token -> token -> none) start
  type four = (token -> token -> token -> none) start
  type ten = none Modifier.two Modifier.eight start
end
module R = Integer_range


module Enemy_kind = struct

  module Kind = struct
    type snake = Snake
    type goblin = Goblin
    type orc = Orc
    type dragon = Dragon
    type ranged = Ranged
    type close_quarter = Close_quarter
  end

  module Template = struct
    open Integer_range
    type snake =
      <kind:Kind.snake; rage:one; health:one >
    type goblin =
      <kind:Kind.goblin; rage:one; health:two >
    type orc =
      <kind:Kind.orc; rage:two; health:two >
    type dragon =
      <kind:Kind.dragon; rage:ten; health:ten >

    type ranged = Ranged
    type close_quarter = Close_quarter
  end

end

module Case = struct
  type door = Door
  type stair = Stair
  type 'a obstacle = Obstacle of 'a
  type 'a enemy = Enemy of 'a
  type 'a floor = Floor of 'a
  type free = Inventory.none floor
  type border = Border
end

module Nat = struct
  type z = Z
  type 'a s = Succ of 'a
end

type f = Case.free
type b = Case.border

type won = Won

type 'a hzip = <
  l: 'a h;
  m: 'a;
  r: 'a h
> col

type eye_view = Eye_view
type main = Physical_position
type 'a event = Event of 'a

type ('a,'b,'c,'d) state =
  < world:'a; player:'b; history:'c; mode:'d>
 
type 'a action_mode = 'm
  constraint 'a = < mode:'m; .. >

type history_start = < path: Previous.none; len:Nat.z >

type player_start =
  <
    inventory: <
      left_hand:Inventory.none;
      right_hand:Inventory.none;
    >;
    exp:Nat.z;
    status : <
      health: Integer_range.two;
      stamina: Integer_range.four;
      mana: Integer_range.two;
    >
  >

module Builder = struct
  open Case
  type _ elt =
    | F: free elt
    | W: border elt
    | S: stair elt
    | K: Inventory.key floor elt
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



type 'a is_free = 'a constraint 'a = 'b Case.floor
type 'a mfree = 'a
  constraint 'a = < m: 'b is_free ; ..>

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

type 'p down = <
  l: 'p l lcdw;
  m: 'p m cdw mfree;
  r: 'p r lcdw
  > cols

type 'p left = <
  l: ('p l, b hzip) pull;
  m: 'p l first mfree;
  r: ('p m, 'p r) push
  > cols

type 'p right = <
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
type 'a player = 'player constraint 'a = <player:'player; ..>


type 'a p = 'player
  constraint 'a = <player: 'player; ..>
type 'a w = 'w
  constraint 'a = <world: 'w; ..>


type ('a,'b, 'c) wmove =
  'a -> <world: 'b; player: 'p; mode:'m;
         history:<len:'len Nat.s; path:('c -> 'path) > >
  constraint 'a =
    < world:'w; player:'p; mode:'m; history:'h >
  constraint 'm = <move:yes; .. >
  constraint 'h = <len:'len; path:'path>


type ('a,'b, 'c) return_move =
  'a -> <world: 'b; player: 'p; mode:'m;
         history:<len:'len; path:'path > >
  constraint 'a =
    < world:'w; player:'p; mode:'m; history:'h >
  constraint 'm = <move:yes; .. >
  constraint 'h = <len:'len Nat.s; path: 'c -> 'path>


type ('a,'b) reset_move =
  'a -> <world: 'b; player: 'p; mode:'m; history:history_start >
  constraint 'a =
    <world:'w; player:'p; mode:'m; history:'h >
  constraint 'm = <move:yes; .. >


type 'a left_hand = 'lh
  constraint
    'a = <inventory:'i; ..>
  constraint
    'i = <left_hand:'lh; .. >
type 'a right_hand = 'x
  constraint 'a = <inventory:'i; .. >
  constraint 'i = <right_hand:'x; ..>
type 'a fl = 'f constraint 'a = 'f Case.floor


type 'a exp = 'exp
  constraint 'a = <exp:'exp; .. >

type 'a history = 'h
  constraint 'a = <history:'h; .. >

type 'a status = 'st
  constraint 'a = <status:'st; .. >

type main_mode = <tag:main; move:yes>
type eye_mode = <tag:eye_view; move:yes>


type 'a pick = <
  world:
    <l:'w l; r:'w r;
     m: <l:'m l;r:'m r;
         m:Case.free
        >
    >;
  player:<
    inventory: <left_hand:'m m fl; right_hand:'a p right_hand>;
    status: 'status;
    exp:'exp
  >;
  history:history_start;
  mode:'mode;
>
  constraint 'w = 'a w
  constraint 'm = 'w m
  constraint 'p = 'a p
  constraint 'p = <status:'status; exp:'exp; .. >
  constraint 'mode = 'a action_mode
  constraint 'mode = main_mode


type 'a swap = <
  world: 'a world;
  player:<left_hand:'a p right_hand fl; right_hand:'a p left_hand; back:Previous.none>
>

type 'a open_door = <
  player:<
    inventory: <left_hand:Inventory.none; right_hand:'rh>;
    status: 'p status;
    exp: 'p exp;
  >;
  world:<
    l:'a world l;
    r: 'a world r;
    m: <l:'a world m l; m:Case.free; r:'a world m r>;
  >;
  history:'a history;
  mode: 'mode
>
  constraint
  'a w m m = Case.door
  constraint
    'p = 'a p
  constraint
    'p = < inventory:<left_hand:Inventory.key; right_hand:'rh>; .. >
  constraint
    'a action_mode = 'mode
  constraint
    'mode = eye_mode

type 'a escape = 'a
  constraint 'a w m m = Case.stair

type 'a eye_switch =
  'a -> < world:'w; mode:eye_mode; history:history_start; player:'p>
  constraint
    'a = <player:'p; world:'w; mode:main_mode; .. >

type 'a main_switch =
  'a -> <player:'p; world:'w; mode:main_mode; history:'h >
  constraint
    'a = < world:'w; mode:eye_mode; history:history_start as 'h; player:'p>


type 'a move =
  | L: ('a, 'a world left, Previous.right ) wmove move
  | U: ('a, 'a world up, Previous.down ) wmove move
  | D: ('a, 'a world down, Previous.up ) wmove move
  | R: ('a,'a world right, Previous.left ) wmove move

  | UL: ('a, 'a world left, Previous.left ) return_move move
  | UU: ('a, 'a world up, Previous.up ) return_move move
  | UD: ('a, 'a world down, Previous.down ) return_move move
  | UR: ('a,'a world right, Previous.right ) return_move move


  | ME: 'a eye_switch move
  | MM: 'a main_switch move

  | P: ('a -> 'a pick) move
  | S: ('a -> 'a swap) move
  | O: ('a -> 'a open_door) move

  | Escape:  ('a escape, won) reset_move move


module type game = sig
  type start
  type _ path =
    | []: start path
    | (::): ('a -> 'b) move * 'a path -> 'b path
end

type 'a game_start =
  <world:'a;
   player: player_start;
   mode: main_mode;
   history: history_start;
  >


let game (type a b c d e f ma mb mc md me mf m mr r)
    (_: <l:a->b->c->d->e->f; m:<l:ma->mb->mc->md->me->mf;r:mr;m:m>; r: r> Builder.bcols):
  (module game
    with type start =
           <l:a->b->c->d->e->f; m:<l:ma->mb->mc->md->me->mf;r:mr;m:m>; r: r> game_start
  )
= (module struct
  type start =
    <l:a->b->c->d->e->f; m:<l:ma ->mb->mc->md->me-> mf;r:mr;m:m>; r: r> game_start
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
  let p = [D;ME;P;D]
  let p = [D;MM;UD;O;D;ME;P;D]
  let path = Escape::D::R::R::U::R::R::R::U::U::R::U::U::U::L::L::L::D::O::D::P::D::[]

end
