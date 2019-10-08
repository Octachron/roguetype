
type empty = Empty
type key = Key
type _ free =
  | Empty: empty free
  | Key: key free
type door = Door

type with_key = With_key
type empty_handed = Empty_handed

type _ player =
  | With_key: with_key player
  | Empty_handed: empty_handed player

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
    typeOf(Ty: <left:e; at: empty free; right:'a; player:empty_handed player> tyw)
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

type 'arg take_key =
  <
    left:'l;
    player: with_key player;
    right:'r;
    at: empty free
  >
constraint 'arg =  <
    left:'l;
    player: empty_handed player;
    right:'r;
    at: key free
  >

type 'arg open_door =
  <left:'l;
   player: empty_handed player;
   right: e * 'r;
   at: 'at >
constraint 'arg = <
  left:'l;
  player: with_key player;
  right: door * 'r;
  at:'at
>


type ('a,'b) move =
  | L: ('arg, 'arg left) move
  | R: ('arg, 'arg right) move
  | K: ('arg, 'arg take_key) move
  | O: ('arg, 'arg open_door) move

type _ play =
  | []: Start.t play
  | (::): ('a,'b) move * 'a play -> 'b play

let s = []
let n1 = [R]
let t = [O;R;K;R]

(*
let win: _ stop play -> unit = function
  | [_;_;_;_;_;_;_] -> .
  | [_;_;_;_;_;_;_;_] -> .
  | [_;_;_;_;_;_;_;_;_] -> .
  | [_;_;_;_;_;_;_;_;_;_] -> .
  | [_;_;_;_;_;_;_;_;_;_;_] -> .
  | [_;_;_;_;_;_;_;_;_;_;_;_] -> .
  | [_;_;_;_;_;_;_;_;_;_;_;_;_] -> .

  | [_] -> .
  | _ -> .
*)
