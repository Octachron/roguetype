
open Rules
open Generic

type player_start =
  <
    inventory: Inventory.none;
    health: Integer_range.three;
  >

module Lvl1 = struct

  type t
  type world
  type start = <world:world; player:player_start; lvl:t; initiative:player_turn >

  type _ path =
    | []: start path
    | (::): ('a -> 'b) move * 'a path -> 'b path

end

module Lvl2(X: sig type p val trace: <lvl:Lv1.t; player:p; level_cleared:yes> Lvl1.path end) =
struct

  type t
  type world
  type start = <world:world; player:X.p; lvl:t; initiative:player_turn >

  type _ path =
    | []: start path
    | (::): ('a -> 'b) move * 'a path -> 'b path

end
