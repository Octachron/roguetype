

type case = Case

type 'a board = 'a
    constraint 'a = < left:'l; at:case; right: 'r >

type start = <left:case; at:case; right: case * (case * (case * (case * case)))> board

type stop = <left:'r; at:case; right: 'l> board
    constraint start = <left:'l; right:'r; .. >

type 'arg right =
  <left:'at*'l; at:'r; right:'r2> board
  constraint 'arg = <left:'l; at:'at; right:'r * 'r2> board

type 'arg left =
  <left:'l2; at:'l; right:'at * 'r> board
  constraint 'arg = <left:'l * 'l2; at:'at; right:'r> board

type ('a,'b) move =
  | L: ('arg, 'arg left) move
  | R: ('arg, 'arg right) move

type _ play =
  | []: start play
  | (::): ('a,'b) move * 'a play -> 'b play


let t = [R]

let win: stop play -> unit = function
  | [_;_;_;_;_] -> .
  | [_] -> .
  | _ -> .
