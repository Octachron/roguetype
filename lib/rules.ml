open Generic

type x = private Tag_x
type player_turn = P
type monster_turn = W

type 'x line =
  < l: 'x -> 'x -> 'x -> 'x -> 'x -> 'x -> 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x -> 'x -> 'x -> 'x -> 'x -> 'x -> 'x >

type mountains = Case.mountain line

type rows_placehoder =
  < up: x -> x -> x -> x -> x -> x -> x -> x;
    m:x;
    dw: x -> x -> x -> x -> x -> x -> x -> x;
  >

type 'x world_placeholder =
  < up:
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x > ->
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x > ->
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x > ->
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x > ->
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x > ->
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x > ->
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x > ->
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x>;
    m:  < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x>;
    dw:
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x > ->
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x > ->
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x > ->
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x > ->
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x > ->
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x > ->
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x > ->
      < l: 'x -> 'x -> 'x -> 'x; m:'x; r:'x -> 'x -> 'x -> 'x>
  >
  
type state_placeholder = <
  lvl:x; init:player_turn;
  player: < health: <current:x; potential:x>; inventory:x >;
  world: x
 > 

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
type m = Case.mountain

type won = Won


type ('a,'b,'c,'d) state =
  < world:'a; player:'b;  init:'c; lvl:'d>
 

type player_start =
  <
    inventory: Inventory.none;
    health: Integer_range.three;
  >

type !_ healing =
  | One: (<potential: Nat.o -> 'a; current:'c> ->  <potential:'a; current: Nat.o -> 'c >) healing
  | More: ('a -> <potential: Nat.o -> 'b; current: 'ch>) healing ->
      ('a -> <potential: 'b; current: Nat.o -> 'ch>) healing


type 'a move =
  | L:
(
<
  lvl:'lvl; init:'init;
  player: 'p;
  world:
    < up:
        <l: 'u1l -> 'u1lr; m:'u1m; r:'u1r > ->
        <l: 'u2l -> 'u2lr; m:'u2m; r:'u2r > ->
        <l: 'u3l -> 'u3lr; m:'u3m; r:'u3r > ->
        <l: 'u4l -> 'u4lr; m:'u4m; r:'u4r > ->
        <l: 'u5l -> 'u5lr; m:'u5m; r:'u5r > ->
        <l: 'u6l -> 'u6lr; m:'u6m; r:'u6r > ->
        <l: 'u7l -> 'u7lr; m:'u7m; r:'u7r > ->
        <l: 'u8l -> 'u8lr; m:'u8m; r:'u8r >
    ;
    m: <l:'ml -> 'mlr; m:'mm Case.floor; r:'mr>;
    dw:
        <l: 'd1l -> 'd1lr; m:'d1m; r:'d1r > ->
        <l: 'd2l -> 'd2lr; m:'d2m; r:'d2r > ->
        <l: 'd3l -> 'd3lr; m:'d3m; r:'d3r > ->
        <l: 'd4l -> 'd4lr; m:'d4m; r:'d4r > ->
        <l: 'd5l -> 'd5lr; m:'d5m; r:'d5r > ->
        <l: 'd6l -> 'd6lr; m:'d6m; r:'d6r > ->
        <l: 'd7l -> 'd7lr; m:'d7m; r:'d7r > ->
        <l: 'd8l -> 'd8lr; m:'d8m; r:'d8r >
    ;
  >
> ->
<
  lvl:'lvl; init:'init;
  player: 'p;
  world:
    < up:
        <l: 'u1lr; m:'u1l; r: 'u1m -> 'u1r > ->
        <l: 'u2lr; m:'u2l; r: 'u2m -> 'u2r > ->
        <l: 'u3lr; m:'u3l; r: 'u3m -> 'u3r > ->
        <l: 'u4lr; m:'u4l; r: 'u4m -> 'u4r > ->
        <l: 'u5lr; m:'u5l; r: 'u5m -> 'u5r > ->
        <l: 'u6lr; m:'u6l; r: 'u6m -> 'u6r > ->
        <l: 'u7lr; m:'u7l; r: 'u7m -> 'u7r > ->
        <l: 'u8lr; m:'u8l; r: 'u8m -> 'u8r >
    ;
    m: <l:'mlr; m:'ml; r: 'mm Case.floor -> 'mr>;
    dw:
        <l: 'd1lr; m:'d1l; r: 'd1m -> 'd1r > ->
        <l: 'd2lr; m:'d2l; r: 'd2m -> 'd2r > ->
        <l: 'd3lr; m:'d3l; r: 'd3m -> 'd3r > ->
        <l: 'd4lr; m:'d4l; r: 'd4m -> 'd4r > ->
        <l: 'd5lr; m:'d5l; r: 'd5m -> 'd5r > ->
        <l: 'd6lr; m:'d6l; r: 'd6m -> 'd6r > ->
        <l: 'd7lr; m:'d7l; r: 'd7m -> 'd7r > ->
        <l: 'd8lr; m:'d8l; r: 'd8m -> 'd8r >
    ;
  >
>
) move

  | R: (<
  lvl:'lvl; init:'init;
  player: 'p;
  world:
    < up:
        <l: 'u1l; m:'u1m; r:'u1r -> 'u1rr > ->
        <l: 'u2l; m:'u2m; r:'u2r -> 'u2rr > ->
        <l: 'u3l; m:'u3m; r:'u3r -> 'u3rr > ->
        <l: 'u4l; m:'u4m; r:'u4r -> 'u4rr > ->
        <l: 'u5l; m:'u5m; r:'u5r -> 'u5rr > ->
        <l: 'u6l; m:'u6m; r:'u6r -> 'u6rr > ->
        <l: 'u7l; m:'u7m; r:'u7r -> 'u7rr > ->
        <l: 'u8l; m:'u8m; r:'u8r -> 'u8rr >
    ;
      m: <l:'ml; m:'mm Case.floor; r:'mr -> 'mrr>;
      dw:
        <l: 'd1l; m:'d1m; r:'d1r -> 'd1rr > ->
        <l: 'd2l; m:'d2m; r:'d2r -> 'd2rr > ->
        <l: 'd3l; m:'d3m; r:'d3r -> 'd3rr > ->
        <l: 'd4l; m:'d4m; r:'d4r -> 'd4rr > ->
        <l: 'd5l; m:'d5m; r:'d5r -> 'd5rr > ->
        <l: 'd6l; m:'d6m; r:'d6r -> 'd6rr > ->
        <l: 'd7l; m:'d7m; r:'d7r -> 'd7rr > ->
        <l: 'd8l; m:'d8m; r:'d8r -> 'd8rr >;
    >
>
 -> <
  lvl:'lvl; init:'init;
  player: 'p;
  world:
    < up:
        <l: 'u1m -> 'u1l; m:'u1r; r: 'u1rr > ->
        <l: 'u2m -> 'u2l; m:'u2r; r: 'u2rr > ->
        <l: 'u3m -> 'u3l; m:'u3r; r: 'u3rr > ->
        <l: 'u4m -> 'u4l; m:'u4r; r: 'u4rr > ->
        <l: 'u5m -> 'u5l; m:'u5r; r: 'u5rr > ->
        <l: 'u6m -> 'u6l; m:'u6r; r: 'u6rr > ->
        <l: 'u7m -> 'u7l; m:'u7r; r: 'u7rr > ->
        <l: 'u8m -> 'u8l; m:'u8r; r: 'u8rr >
    ;
      m: <l:'mm Case.floor -> 'ml; m:'mr; r: 'mrr>;
      dw:
        <l: 'd1m -> 'd1l; m:'d1r; r: 'd1rr > ->
        <l: 'd2m -> 'd2l; m:'d2r; r: 'd2rr > ->
        <l: 'd3m -> 'd3l; m:'d3r; r: 'd3rr > ->
        <l: 'd4m -> 'd4l; m:'d4r; r: 'd4rr > ->
        <l: 'd5m -> 'd5l; m:'d5r; r: 'd5rr > ->
        <l: 'd6m -> 'd6l; m:'d6r; r: 'd6rr > ->
        <l: 'd7m -> 'd7l; m:'d7r; r: 'd7rr > ->
        <l: 'd8m -> 'd8l; m:'d8r; r: 'd8rr >
    ;
    >
>) move


  | U: (<
  lvl:'lvl; init:'init;
  player: 'p;
  world: <
    up: 'u1 -> 'u2 -> 'u3 -> 'u4 -> 'u5 -> 'u6 -> 'u7 -> 'u8;
    m: <l:'l; m:'mm Case.floor; r:'r>;
    dw: 'd1 -> 'd2 -> 'd3 -> 'd4 -> 'd5 -> 'd6 -> 'd7 -> 'd8;
    >
> -> <
  lvl:'lvl; init:'init;
  player: 'p;
  world: <
    up: 'u2 -> 'u3 -> 'u4 -> 'u5 -> 'u6 -> 'u7 -> 'u8
      -> mountains;
    m: 'u1;
    dw:
      <l:'l; m:'mm Case.floor; r:'r>
      -> 'd1 -> 'd2 -> 'd3 -> 'd4 -> 'd5 -> 'd6 -> 'd7;
    >
>)move

  | D:(<
  lvl:'lvl; init:'init;
  player: 'p;
  world: <
    up: 'u1 -> 'u2 -> 'u3 -> 'u4 -> 'u5 -> 'u6 -> 'u7 -> 'u8;
    m: <l:'l; m:'mm Case.floor; r:'r>;
    dw: 'd1 -> 'd2 -> 'd3 -> 'd4 -> 'd5 -> 'd6 -> 'd7 -> 'd8;
    >
> -> <
  lvl:'lvl; init:'init;
  player: 'p;
  world: <
    up: <l:'l; m:'mm Case.floor; r:'r> ->
      'u1 -> 'u2 -> 'u3 -> 'u4 -> 'u5 -> 'u6 -> 'u7;
    m: 'd1;
    dw:
      'd2 -> 'd3 -> 'd4 -> 'd5 -> 'd6 -> 'd7 -> 'd8
       -> mountains;
    >
>)move


  | P: (
<
  lvl:'lvl; init:'init;
  player: < health: 'h; inventory:'pi >;
  world: < up:'up; m:<l:'l; m: 'item Case.floor; r:'r >; dw:'d >
> ->
<
  lvl:'lvl; init:'init;
  player: < health: 'h; inventory:'item >;
  world: < up:'up; m:<l:'l; m: 'pi Case.floor; r:'r >; dw:'d >
>
) move
  | O: (
<
  lvl:'lvl; init:'init;
  player: < health: 'h; inventory:Inventory.key >;
  world: < up:'up; m:<l:'l; m: Case.door; r:'r >; dw:'d >
> ->
<
  lvl:'lvl; init:'init;
  player: < health: 'h; inventory:Inventory.none >;
  world: < up:'up; m:<l:'l; m: Case.free; r:'r >; dw:'d >
>
) move
  | C: (
<
  lvl:'lvl; init:'init;
  player: < health: 'h; inventory:Inventory.axe >;
  world: < up:'up; m:<l:'l; m: Case.forest; r:'r >; dw:'d >
> ->
<
  lvl:'lvl; init:'init;
  player: < health: 'h; inventory:Inventory.axe >;
  world: < up:'up; m:<l:'l; m: Case.free; r:'r >; dw:'d >
>
) move
  | Dp: ('h -> 'nh) healing ->
      (<
        lvl:'lvl; init:'init;
        player:<health:'h; inventory:Inventory.potion>;
        world:'w
        > ->
       <
        lvl:'lvl; init:'init;
        player:<health:'nh; inventory:Inventory.none>;
        world:'w
        >) move

  | E:
      (<
        lvl:'lvl; init:'init;
        player:<health:<current:'hc; potential:'hp>; inventory:Inventory.elixir>;
        world:'w
        > ->
       <
        lvl:'lvl; init:'init;
        player:<
          health: <current:Nat.o -> 'hc; potential:'hp>;
          inventory:Inventory.none
        >;
        world:'w
        >) move

  | A: (
<
  lvl:'lvl; init:'init;
  player: <inventory:Inventory.ring_of_annihilation; health:_>;
  world: <
    up: <l:'u1l; m:_; r:'u1r> -> <l:'u2l; m:_; r:'u2r> -> 'ur;
    m: <l:_ -> _ -> 'mlr; m:_; r: _ -> _ -> 'mrr >;
    dw: <l:'d1l; m:_; r:'d1r> -> <l:'d2l; m:_; r:'d2r> -> 'dr;
  >
> -> <
  lvl:'lvl; init:'init;
  player: <inventory:Inventory.none; health:<current:Nat.o -> Nat.z; potential:Nat.z > >;
  world: <
    up: <l:'u1l; m:Case.free; r:'u1r> -> <l:'u2l; m:Case.free; r:'u2r> -> 'ur;
    m: <l:Case.free -> Case.free -> 'mlr; m:_; r: Case.free -> Case.free -> 'mrr >;
    dw: <l:'d1l; m:Case.free; r:'d1r> -> <l:'d2l; m:Case.free; r:'d2r> -> 'dr;
  >
>) move

  | FS: (<
  lvl: 'lvl;
  init:player_turn;
  player: <inventory:'i; health: <current:Nat.o -> 'hc; potential:'hp> >;
  world: <
    up:'u;
    m:<l:'l; m: <health: Nat.o -> 'mh> Case.monster; r:'r >;
    dw:'d
  >;
> -> <
  lvl: 'lvl;
  init:monster_turn;
  player: <inventory:'i; health: <current:Nat.o -> 'hc; potential:'hp> >;
  world: <
    up:'u;
    m:<l:'l; m: <health: 'mh> Case.monster; r:'r >;
    dw:'d
  >;
  >) move
  | FMS: (<
  lvl: 'lvl;
  init:player_turn;
  player: <inventory:Inventory.mithril_sword; health: <current:Nat.o -> 'hc; potential:'hp> >;
  world: <
    up:'u;
    m:<l:'l; m: <health: Nat.o -> Nat.o -> 'mh> Case.monster; r:'r >;
    dw:'d
  >;
> -> <
  lvl: 'lvl;
  init:monster_turn;
  player: <inventory:Inventory.mithril_sword; health: <current:Nat.o -> 'hc; potential:'hp> >;
  world: <
    up:'u;
    m:<l:'l; m: <health: 'mh> Case.monster; r:'r >;
    dw:'d
  > >) move

  | FCS: (<
  lvl: 'lvl;
  init:player_turn;
  player: <inventory:Inventory.cristal_sword; health: <current:Nat.o -> 'hc; potential:'hp> >;
  world: <
    up:'u;
    m:<l:'l; m:_ Case.monster; r:'r >;
    dw:'d
  > > -> <
  lvl: 'lvl;
  init:player_turn;
  player: <inventory:Inventory.none; health: <current:Nat.o -> 'hc; potential:'hp> >;
  world: <
    up:'u;
    m:<l:'l; m: Case.free; r:'r >;
    dw:'d
  > >) move

  | Df:
 (<
  lvl: 'lvl;
  init:monster_turn;
  player: <inventory:'i; health: <current:Nat.o -> 'hc; potential:'hp> >;
  world: <
    up:'u;
    m:<l:'l; m: <health: Nat.o -> 'mh> Case.monster; r:'r >;
    dw:'d;
  > > -> <
  lvl: 'lvl;
  init:player_turn;
  player: <inventory:'i; health: <current:'hc; potential: Nat.o -> 'hp> >;
  world: <
    up:'u;
    m:<l:'l; m: <health: Nat.o -> 'mh> Case.monster; r:'r >;
    dw:'d;
  > >) move

  | K: (<
  lvl: 'lvl;
  init:monster_turn;
  player: <inventory:'i; health: <current:Nat.o -> 'hc; potential:'hp> >;
  world: <
    up:'u;
    m:<l:'l; m: <health: Nat.z> Case.monster; r:'r >;
    dw:'d;
  > > -> <
  lvl: 'lvl;
  init:player_turn;
  player: <inventory:'i; health: <current:Nat.o -> 'hc; potential:'hp> >;
  world: <
    up:'u;
    m:<l:'l; m: Case.free; r:'r >;
    dw:'d
  > >) move

  | Gate: (<
    lvl: 'lvl;
    init:_;
    player: 'p;
    world: <
      up:_;
      m:<l:_; m: Case.gate; r:_ >;
      dw:_
    >
  > -> <
  lvl: 'lvl;
  level_cleared:yes;
  player: 'p;
  >) move
  | Win: (<
    lvl: _;
    init:_;
    player: _;
    world: <
      up:_;
      m:<l:_; m: Case.altar; r:_ >;
      dw:_
    >
  > -> < victory:yes >) move
