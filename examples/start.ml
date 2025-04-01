open Roguetype_lib

open Game.Lvl1

let start = []

let (=>) l x = x :: l

let v = start => D => D => D => R => R => D  => R  => R
        => U => U => U => U => U => U => U => U => Gate

module Trace_1 = struct
  type lvl = Game.Lvl1.t
  type p= Game.player_start
  type 'a path = 'a Game.Lvl1.path
  let trace = v
end

module L2 = Game.Lvl2(Trace_1)
open L2

let[@warning "-32"] start_2 = []

(*
let w = v
        => R => R => D => R => R
        => U => U => U => U => U => U => U => U => G
*)
