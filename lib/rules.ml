open Generic

type  'a zip = 'a constraint
  'a = <l:'l; m:'m; r:'r>

type 'a h = 'a -> 'a -> 'a -> 'a -> 'a -> 'a -> 'a -> 'a

type 'p col = 'p constraint
  'p = <
    l:'l1 -> 'l2 -> 'l3 -> 'l4 -> 'l5 -> 'l6 -> 'l7 -> 'l8;
    m:'mid;
    r: 'r1 -> 'r2 -> 'r3 -> 'r4 -> 'r5 -> 'r6 -> 'r7 -> 'r8
  > zip

type 'p cols = 'p constraint
  'p = <
    l:'l1 col -> 'l2 col -> 'l3 col -> 'l4 col -> 'l5 col -> 'l6 col -> 'l7 col -> 'l8 col;
    m:'mid col;
    r: 'r1 col -> 'r2 col -> 'r3 col -> 'r4 col -> 'r5 col -> 'r6 col -> 'r7 col -> 'r8 col
  > zip

module Integer_range = struct

  type o = Generic.Nat.o
  type z = Generic.Nat.z
  module Modifier = struct
    type 'a one = o -> 'a
    type 'a two = 'a one one
    type 'a three = 'a one two
    type 'a four = 'a two two
    type 'a eight = 'a four four
  end


  type 'a start = <current:'a; potential: z>

  type one = (o -> z) start
  type two = (o -> o -> z) start
  type three = (o -> o -> o -> z) start
  type four = (o -> o -> o -> z) start
  type ten = z Modifier.two Modifier.eight start
end
module R = Integer_range


type f = Case.free
type b = Case.border

type won = Won

type 'a hzip = <
  l: 'a h;
  m: 'a;
  r: 'a h
> col

type ('a,'b,'c,'d) state =
  < world:'a; player:'b;  init:'c; lvl:'d>
 
type 'a init = 'm
  constraint 'a = < init:'m; .. >

type 'a i = 'i constraint 'a =  <inventory:'i; .. >

type 'a health = 'h
  constraint 'a = < health:'h; .. >


type player_start =
  <
    inventory: Inventory.none;
    health: Integer_range.three;
  >

module Builder = struct
  open Case
  open Inventory
  open Monster
  type _ elt =
    | F: free elt
    | M: mountain elt
    | T: forest elt
    | G: gate elt
    | Altar: altar elt
    | D: door elt
    | R: ring_of_annihilation elt

    | K: key floor elt
    | H: potion floor elt
    | E: elixir floor elt
    | MS: mithril_sword floor elt
    | CS: cristal_sword floor elt
    | Ax: axe floor elt

    | Ko: kobold elt
    | Go: goblin elt
    | O: orc elt
    | Og: ogre elt
    | Dr: dragon elt


  type 'a l =
    | []: (border -> border -> border) l
    | (::): 'a elt * 'b l -> ('a -> 'b) l

  type 'p inv =  'b -> 'a -> 'c -> 'd
    constraint 'p = 'a -> 'b -> 'c -> 'd

  type 'p bcol = 'l inv l * 'm elt * 'r l
    constraint 'p = <l:'l; m:'m; r:'r >



  type 'a ll =
    | []: (border hzip -> border hzip -> border hzip) ll
    | (::): 'p bcol * 'b ll -> ('p -> 'b) ll

  type 'p bcols = 'l inv ll * 'm bcol * 'r ll
    constraint 'p = <l:'l; m:'m; r:'r >

end



type 'p first = 'x1
  constraint 'p = 'x1 -> 'x2 -> 'x3 -> 'x4 -> 'x5 -> 'x6 -> 'x7 -> 'x8

type ('p,'y) app = 'x1 -> 'x2 -> 'x3 -> 'x4 -> 'x5 -> 'x6 -> 'x7 -> 'y
  constraint 'p = 'x1 -> 'x2 -> 'x3 -> 'x4 -> 'x5 -> 'x6 -> 'x7 -> 'x8


type ('p,'y) pull = 'x2 -> 'x3 -> 'x4 -> 'x5 -> 'x6 -> 'x7 -> 'x8 -> 'y
  constraint 'p = 'x1 -> 'x2 -> 'x3 -> 'x4 -> 'x5 -> 'x6 -> 'x7 -> 'x8

type ('y,'p) push = 'y -> 'x1 -> 'x2 -> 'x3 -> 'x4 -> 'x5 -> 'x6 -> 'x7
  constraint 'p =  'x1 -> 'x2 -> 'x3 -> 'x4 -> 'x5 -> 'x6 -> 'x7 -> 'x8


type 'a mfree = 'a
  constraint 'a = < m: _ Case.floor ; ..>

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

type 'p lcup = 'c1 cup -> 'c2 cup -> 'c3 cup -> 'c4 cup -> 'c5 cup -> 'c6 cup -> 'c7 cup -> 'c8 cup
    constraint 'p = 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'c6 -> 'c7 -> 'c8


type 'p lcdw = 'c1 cdw -> 'c2 cdw -> 'c3 cdw -> 'c4 cdw -> 'c5 cdw -> 'c6 cdw -> 'c7 cdw -> 'c8 cdw
    constraint 'p = 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'c6 -> 'c7 -> 'c8


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

type ('f,'b) line = 'f -> 'f -> 'f -> 'f -> 'b -> 'b -> 'b -> 'b

type ('a,'b) scol = < l: 'a ;m:'b; r:'a> col


type mcol = ((f,b) line, f) scol

type start = (
  (mcol, b hzip) line,
  mcol
) scol

type 'a world = 'world constraint 'a = <world:'world; ..>
type 'a player = 'player constraint 'a = <player:'player; ..>

type player_turn = P
type monster_turn = W

type 'a p = 'player
  constraint 'a = <player: 'player; ..>
type 'a w = 'w
  constraint 'a = <world: 'w; ..>


type 'a lvl = 'l constraint 'a = <lvl:'l; ..>

type 'a fl = 'f constraint 'a = 'f Case.floor

type ('a,'b) wmove =
  'a -> <world: 'b; player: 'a p; init:'a init; lvl: 'a lvl >
  constraint 'a w m m fl = _


type 'a pick = <
  world:
    <l:'w l; r:'w r;
     m: <l:'m l;r:'m r;
         m:'p i
        >
    >;
  player:<
    inventory: 'm m;
    health: 'p h
  >;
  init: 'a init;
  lvl:'a lvl;
>
  constraint 'w = 'a w
  constraint 'm = 'w m
  constraint 'p = 'a p



type 'a open_door = <
  player:<
    inventory: Inventory.none;
    health: 'p health
  >;
  world:<
    l:'a world l;
    r: 'a world r;
    m: <l:'a world m l; m:Case.free; r:'a world m r>;
  >;
  init: 'a init;
  lvl: 'a lvl
>
  constraint
    'a w m m = Case.door
  constraint
    'p = 'a p

type 'a forest_cut = <
  player: 'a p;
  world:<
    l:'a world l;
    r: 'a world r;
    m: <l:'a world m l; m:Case.free; r:'a world m r>;
  >;
  init: 'a init;
  lvl: 'a lvl
>
  constraint
    'a w m m = Case.forest
  constraint
    'a p i = Inventory.axe

type 'a defend =
  < player: < health: <current:'hc; potential: R.o ->  'hp  >; inventory: 'p i >;
    world:'a w;
    init:player_turn;
    lvl: 'a lvl
  >
  constraint
    'p = 'a p
  constraint
    'p health = <current: R.o -> 'hc; potential: 'hp >

type 'a kill =
  < player: 'p;
    world:<
      l:'a world l;
      r: 'a world r;
      m: <l:'a world m l; m:Case.free; r:'a world m r>;
    >;
    init:player_turn;
    lvl: 'a lvl
  >
  constraint
    'p = 'a p
  constraint
   'a init = player_turn
  constraint
    'a w m m health = Nat.z



type 'a drink_elixir =
  < player: < health: <current: R.o -> 'hc; potential: 'hp  >; inventory: 'p i >;
    world:'a w;
    init:player_turn;
    lvl: 'a lvl
  >
  constraint
    'p = 'a p
  constraint
    'p health = <current:R.o-> 'hc; potential: 'hp >

type 'a sword_attack =
  <  world:<
      l:'a world l;
      r: 'a world r;
      m: <l:'a world m l; m:'mh Case.monster; r:'a world m r>;
    >;
    player: 'a p;
    lvl: 'a lvl;
    init:monster_turn
  >
  constraint
    'a w m m = (Nat.o -> 'mh) Case.monster


type 'a mithril_sword_attack =
  <  world:<
      l:'a world l;
      r: 'a world r;
      m: <l:'a world m l; m:'mh Case.monster; r:'a world m r>;
    >;
    player: 'a p;
    lvl: 'a lvl;
    init:monster_turn
  >
  constraint
    'a w m m = (Nat.o -> Nat.o -> 'mh) Case.monster
  constraint
    'a p i = Inventory.mithril_sword


type 'a cristal_sword_attack =
  <  world:<
      l:'a world l;
      r: 'a world r;
      m: <l:'a world m l; m:Case.free; r:'a world m r>;
    >;
    player: <health: 'a p health; inventory:Inventory.none >;
    lvl: 'a lvl;
    init:monster_turn
  >
  constraint
    'a w m m = _ Case.monster
  constraint
    'a p i = Inventory.cristal_sword

type 'a gate =
  < lvl_cleared:yes;
    lvl: 'a lvl;
    player: 'a p
  >
  constraint
    'a w m m = Case.gate

type 'a victory =
  < victory:yes >
  constraint
    'a w m m = Case.altar


type !_ healing =
  | One: (<potential: Nat.o -> 'a; current:'c> ->  <potential:'a; current: Nat.o -> 'c >) healing
  | More: ('a -> <potential: Nat.o -> 'b; current: 'ch>) healing ->
      ('a -> <potential: 'b; current: Nat.o -> 'ch>) healing

type ('a,'h, 'r) drink_potion =
  <
    world:'a w;
    init: 'a init;
    lvl: 'a lvl;
    player: < inventory:'a p i; health: 'r >
  >
 constraint
   'h = 'a p health -> 'r


type 'a annihilation =
  < player: <inventory:Inventory.none; health: <current: Nat.o -> Nat.z; potential: Nat.z > >;
    init:player_turn;
    lvl:'a lvl;
    world : <
      l: <l:'ul1; m:Case.free; r:'ur1> -> <l:'ul2; m:Case.free; r:'ur2> -> 'u3;
      m:<l: Case.free -> Case.free -> 'l; m: Case.free; r: Case.free -> Case.free -> 'r>;
      r: <l:'dl1; m:Case.free; r:'dr1> -> <l:'dl2; m:Case.free; r:'dr2> -> 'd3;
    >
  >
  constraint
    'a w = <
    l: <l:'ul1; m:_; r:'ur1> -> <l:'ul2; m:_; r:'ur2> -> 'u3;
    m:<l: _ -> _ -> 'l; m: _; r: _ -> _ -> 'r>;
    r: <l:'dl1; m:_; r:'dr1> -> <l:'dl2; m:_; r:'dr2> -> 'd3;
  >

type 'a move =
  | L: ('a, 'a world left ) wmove move
  | U: ('a, 'a world up) wmove move
  | D: ('a, 'a world down) wmove move
  | R: ('a,'a world right) wmove move

  | P: ('a -> 'a pick) move
  | O: ('a -> 'a open_door) move
  | C: ('a -> 'a forest_cut) move
  | Dp: ('h -> 'nh) healing -> ('a -> ('a,'h, 'nh) drink_potion) move
  | E: ('a -> 'a drink_elixir) move
  | A: ('a -> 'a annihilation) move

  | FS: ('a -> 'a sword_attack) move
  | FMS: ('a -> 'a mithril_sword_attack) move
  | FCS: ('a -> 'a cristal_sword_attack) move

  | Df: ('a -> 'a defend) move
  | K: ('a -> 'a kill) move

  | Gate:  ('a -> 'a gate) move
  | Win: ('a -> 'a victory) move

