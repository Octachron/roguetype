
type empty = Empty
type key = Key
type _ free =
  | Empty: empty free
  | Key: key free
type door = Door


type 'p player = <left_hand:'a; right_hand:'b>
  constraint 'p = 'a * 'b

type ('a,'b) state = <self: 'a player; case: 'b free >
type _ case =
  | Case: empty free case
  | Key: key free case
  | Door: door case

type 'a board = 'a
    constraint 'a = < left:'l; player:'p player; at:'c free; right: 'r >

type e = empty free
type k = key free
type d = door

module type T = sig type t end
type 'a ty = (module T with type t = 'a)
type 'a tyw = Ty

module Builder = struct
  type _ t =
    | []: e t
    | (::): 'a case * 'b t -> ('a * 'b) t


  let typeOf (type x) (_: x tyw): x ty =
    (module struct type t = x end)
  let start (x:'a t) =
    typeOf(Ty: <left:e; at: empty free; right:'a; player:(empty * empty) player> tyw)
end

let st = Builder.[Case;Key;Key;Case;Door;Door;Case]
module Start = (val Builder.start st)

type 'a stop = <right: e; .. >  as 'a
type 'arg right =
  <
    left:'at free *'l
  ; player:'p
  ; at:'r free
  ; right:'r2
  > board
  constraint 'arg = <
    left:'l;
    player:'p;
    at:'at free;
    right:'r free  * 'r2
  > board

type 'arg left =
  <left:'l2; at:'l free; right:'at free * 'r; player:'p> board
  constraint 'arg =
    <left:
       'l free * 'l2;
     at:'at free;
     right:'r;
     player:'p;
    > board

type 'arg take =
  <
    left:'l;
    player: <left_hand:'on_floor; right_hand:'lh>;
    right:'r;
    at: 'rh free
  >
constraint 'arg =  <
    left:'l;
    player: <left_hand:'lh; right_hand:'rh>;
    right:'r;
    at: 'on_floor free
  >

type 'arg open_door =
  <left:'l;
   player: <left_hand:empty; right_hand: 'rh>;
   right: e * 'r;
   at: 'at >
constraint 'arg = <
  left:'l;
  player: <left_hand:key; right_hand:'rh>;
  right: door * 'r;
  at:'at
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
type 'a the_end = winning constraint 'a = <right:e; ..>



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
let n1 = [R]
let t = [End;R;R;O;R;S;O;R;T;R;T;R;R]
