[@@@warning "-32"]

open Roguetype_lib
open! Generic
open! Rules

module Lvl1 = struct
  include Game.Lvl1

  type p = Game.player_start

  let trace =
    start => D => D => D => R => R => D => R => R => U => U => U => U => U => U
    => U => U => Gate
end

module Lvl2 = struct
  include Game.Lvl2 (Lvl1)

  (* type p = < health : R.four ; inventory : Inventory.none > *)

  (* let trace = *)
  (*   start => R => R => D => R => R => U => U => U => U => U => U => U => U => G *)
end
