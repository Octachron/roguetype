
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

type e = empty free case
type k = key free case
type d = door case

type start = <left:e;
              at:empty free;
              player: empty_handed player;
              right: e * (k * (d * (e * e)))> board

type 'a stop = <right: e; .. >  as 'a
type 'arg right =
  <
    left:'at free case*'l
  ; player:'p
  ; at:'r free
  ; right:'r2
  > board
  constraint 'arg = <
    left:'l;
    player:'p;
    at:'at free;
    right:'r free case * 'r2
  > board

type 'arg left =
  <left:'l2; at:'l free; right:'at free case * 'r; player:'p> board
  constraint 'arg =
    <left:
       'l free case* 'l2;
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
  right: door case * 'r;
  at:'at
>


type ('a,'b) move =
  | L: ('arg, 'arg left) move
  | R: ('arg, 'arg right) move
  | K: ('arg, 'arg take_key) move
  | O: ('arg, 'arg open_door) move

type _ play =
  | []: start play
  | (::): ('a,'b) move * 'a play -> 'b play

let s = []
let n1 = [R]
let t = [K;R;R]

let win: _ stop play -> unit = function
  | [_;_;_;_;_;_;_] -> .
  | [_] -> .
  | _ -> .
