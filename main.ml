
type empty = Empty
type key = Key
type _ free =
  | Empty: empty free
  | Key: key free
type door = Door
type wall = Wall

type 'p player = <left_hand:'a; right_hand:'b>
  constraint 'p = 'a * 'b

type 'a case = 'a
  constraint 'a = <now:_; after: _; back: _ >

type 'a hcase = <now:'a; after:'a; back:'a> case

type cdoor = <now:door free; after:wall; back:door free>

type ('a,'b) state = <self: 'a player; case: 'b free >

type _ case_builder =
  | Case: empty free hcase case_builder
  | Key: key free hcase case_builder
  | Door: cdoor case_builder

type 'a board = 'a
    constraint 'a = < left:'l; player:'p player; at:'c case; right: 'r >

type e = empty free
type k = key free
type d = door

module type T = sig type t end
type 'a ty = (module T with type t = 'a)
type 'a tyw = Ty

module Builder = struct
  type _ t =
    | []: e t
    | (::): 'a case_builder * 'b t -> ('a * 'b) t


  let typeOf (type x) (_: x tyw): x ty =
    (module struct type t = x end)
  let start ~left:(x:'a t) ~right:(y:'b t) =
    typeOf(Ty: <left:'a; at: empty free hcase; right:'b; player:(empty * empty) player> tyw)
end

let left = Builder.[Case;Door; Key;Key;Case;Case]
let right = Builder.[Case; Key; Door; Door; Case;Case]
module Start = (val Builder.start ~left ~right)

type 'a stop = <right: e; .. >  as 'a
type 'arg right =
  <
    left:'at * ( <now:'l1; after:'la2; back:'l1> * 'l)
  ; player:'p
  ; at: <back:'back free; now:'back free; after:'after>
  ; right:<now:'after; back:'r2; after:'a2> * 'r3
  > board
  constraint 'arg = <
    left:<back:'l1; after:'la2; ..> * 'l;
    player:'p;
    at:'at;
    right:<now:'r free;after:'after; back:'back free> * ( <now:'r2; after:'a2; ..> * 'r3)
  > board

type 'arg left =
  <left: <now:'la; back:'l2; after: 'l2a> * 'l3;
   at:  <back:'back free; now:'back free; after:'la>;
   right:'at * (<now:'r1b; back:'r1b; after:'r1a> * 'r2);
   player:'p
  > board
  constraint 'arg =
    <left: <now:'l free; after:'la; back:'back free> * (<now:'l2; back:_; after:'l2a> * 'l3);
     at:'at;
     right: <now:_; back:'r1b; after:'r1a> * 'r2;
     player:'p;
    > board

type 'arg take =
  <
    left:'l;
    player: <left_hand:'on_floor; right_hand:'lh>;
    right:'r;
    at: 'rh free hcase
  >
constraint 'arg =  <
    left:'l;
    player: <left_hand:'lh; right_hand:'rh>;
    right:'r;
    at: 'on_floor free hcase
  >

type 'arg open_door =
  <left: <now:'lb; after:'la; back:'lb> * 'l;
   player: <left_hand:empty; right_hand: 'rh>;
   right: <now:'rb; after:'ra; back:'rb> * 'r;
   at: e hcase >
constraint 'arg = <
  left: <back:'lb; after:'la; ..> * 'l;
  player: <left_hand:key; right_hand:'rh>;
  right: <back:'rb; after:'ra; ..>  * 'r;
  at: <now:door free; ..>
>

type 'arg swap =
  <
    left:'l;
    player: <left_hand:'rh; right_hand:'lh>;
    right:'r;
    at: 'at
  >
constraint 'arg =  <
    left:'l;
    player: <left_hand:'lh; right_hand:'rh>;
    right:'r;
    at: 'at
  >

type winning = Win
type 'a the_end = winning constraint 'a = <right:_ * e; ..>



type ('a,'b) move =
  | L: ('arg, 'arg left) move
  | R: ('arg, 'arg right) move
  | T: ('arg, 'arg take) move
  | S: ('arg, 'arg swap) move
  | O: ('arg, 'arg open_door) move
  | End: ('arg, 'arg the_end) move

type _ play =
  | []: Start.t play
  | (::): ('a,'b) move * 'a play -> 'b play

let s = []

let n1 = [O;R;R;R;R;R;R;R;T;L;T;L;O;L;L;L;L;T;R;R]
let n2 =   End :: R :: O :: S :: R :: n1
