
open Rules
open Generic

type player_start =
  <
    inventory: Inventory.none;
    health: Integer_range.four;
  >

type ('c1,'c2,'c3,'c4,'c5,'c6,'c7,'c8,'c9) row =
  <l:'c4 -> 'c3 -> 'c2 -> 'c1 -> m; m:'c5; r:'c6 -> 'c7 -> 'c8 -> 'c9 -> m >

type many_mountains = mountains -> mountains -> mountains -> mountains

type ('r1,'r2,'r3,'r4,'r5,'r6,'r7,'r8,'r9) grid = <
  up:'r4 -> 'r3 -> 'r2 -> 'r1 -> many_mountains;
  m:'r5;
  dw:'r6 -> 'r7 -> 'r8 -> 'r9 -> many_mountains
>


module Short_cases = struct
  open Case
  type m = mountain
  type t = forest
  type f = free
  type d = door
  type g = gate
  type al = altar

  type k = key
  type a = axe
  type p = potion
  type e = elixir
  type s = mithril_sword
  type c = cristal_sword
  type r = ring_of_annihilation

  type ko = kobold
  type go = goblin
  type oc = orc
  type og = ogre
  type dr = dragon
  type tr = troll

end

module Lvl1 = struct
  open Short_cases
  type t
  type world = (
    (a, f, f, t, f, f, f, t, g) row,
    (t, t, f, t, f, t, t, t, f) row,
    (f, f, f, t, f, t, t, t, f) row,
    (f, t, t, t, f, t, f, t, f) row,
    (f, t, f, f, f, f, f, t, f) row,
    (f, t, t, f, f, t, f, t, f) row,
    (f, f, f, t, f, t, f, t, f) row,
    (t, t, f, t, f, f, f, t, f) row,
    (e, t, f, f, f, t, f, f, f) row
  ) grid

  type start = <world:world; player:player_start; lvl:t; init:player_turn >

  type _ path =
    | []: start path
    | (::): ('a -> 'b) move * 'a path -> 'b path

end

module type clear = sig
  type 'a path
  type p
  type lvl
  val trace: <player:p; level_cleared:yes; lvl: lvl > path
end

module Lvl2(X: clear with type 'a path = 'a Lvl1.path) =
struct

  type t
  open Short_cases
  type world = (
    (f, f, f, f, f, f, m, m, g) row,
    (f, m, m, f, m, f, m, m, f) row,
    (f, m, m, f, m, f, m, m, d) row,
    (f, m, m, f, f, f, m, m, f) row,
    (f, f, f, f, f, f, f, f, f) row,
    (m, m, m, f, f, f, m, m, m) row,
    (m, m, m, m, f, f, f, f, f) row,
    (m, k, m, m, m, m, m, m, f) row,
    (m, f, f, f, f, f, f, f, f) row
  ) grid
  type start = <world:world; player:X.p; lvl:t; init:player_turn >

  type _ path =
    | []: start path
    | (::): ('a -> 'b) move * 'a path -> 'b path
end


module Lvl3(L1: clear with type 'a path = 'a Lvl1.path)(X: clear with type 'a path = 'a Lvl2(L1).path) =
struct

  type t
  open Short_cases
  type world = (
    (f , f , f , f , f , ko, f , f , f ) row,
    (f , t , t , go, t , t , t , t , f ) row,
    (f , t , f , f , f , f , go, t , go) row,
    (f , t , f , t , f , f , f , t , f ) row,
    (f , f , f , ko, f , go, f , go, f ) row,
    (f , t , t , t , f , f , f , t , f ) row,
    (f , f , f , f , ko, f , f , t , ko) row,
    (t , t , t , t , go, t , t , t , f ) row,
    (g , f , go, f , f , ko, f , f , f ) row
  ) grid
  type start = <world:world; player:X.p; lvl:t; init:player_turn >

  type _ path =
    | []: start path
    | (::): ('a -> 'b) move * 'a path -> 'b path
end


module Lvl4
    (L1: clear with type 'a path = 'a Lvl1.path)
    (L2: clear with type 'a path = 'a Lvl2(L1).path)
    (X: clear with type 'a path = 'a Lvl3(L1)(L2).path) =
struct

  type t
  open Short_cases
  type world = (
   (d , f, f, m, k, f, f, m, g) row,
   (s, m, go, m, m, m, ko, m, oc) row,
   (m, m, f, f, go, f, f, m, go) row,
   (f, ko, f, f, m, m, m, m, go) row,
   (f, m, f, f, f, f, go, f, f) row,
   (go, go, go, m, m, m, f, m, m) row,
   (f, f, f, m, ko, f, f, m, k) row,
   (m, m, go, m, f, m, m, m, f) row,
   (p, d, f, m, f, f, f, go, f) row
  ) grid

  type start = <world:world; player:X.p; lvl:t; init:player_turn >

  type _ path =
    | []: start path
    | (::): ('a -> 'b) move * 'a path -> 'b path
end

module Lvl5
    (L1: clear with type 'a path = 'a Lvl1.path)
    (L2: clear with type 'a path = 'a Lvl2(L1).path)
    (L3: clear with type 'a path = 'a Lvl3(L1)(L2).path)
    (X: clear with type 'a path = 'a Lvl4(L1)(L2)(L3).path)
= struct

  type t
  open Short_cases
  type world = (
    (m, m, k, m, f, oc, og, tr, c) row,
    (g, m, f, m, f, m, m, m, m ) row,
    (p, m, oc, m, f, f, go, m, k) row,
    (d, m, f, f, f, f, m, f, oc) row,
    (go, m, f, f, f, f, f, f, m) row,
    (f, f, f, f, f, m, m, f, f) row,
    (f, m, go, m, f, f, f, m, oc) row,
    (f, f, m, p, m, f, f, m, f) row,
    (f, f, f, d, m, f, f, m, e) row
  ) grid
  type start = <world:world; player:X.p; lvl:t; init:player_turn >

  type _ path =
    | []: start path
    | (::): ('a -> 'b) move * 'a path -> 'b path
end

module Lvl6
    (L1: clear with type 'a path = 'a Lvl1.path)
    (L2: clear with type 'a path = 'a Lvl2(L1).path)
    (L3: clear with type 'a path = 'a Lvl3(L1)(L2).path)
    (L4: clear with type 'a path = 'a Lvl4(L1)(L2)(L3).path)
    (X: clear with type 'a path = 'a Lvl5(L1)(L2)(L3)(L4).path)
= struct

  type t
  open Short_cases
  type world = (
    (t, t, t, t, t, t, tr, t, f) row,
    (t, e, t, t, g, t, f, t, oc) row,
    (t, tr, t, t, t, t, f, t, f) row,
    (f, f, f, t, f, f, f, t, f) row,
    (ko, t, f, t, f, t, t, t, f) row,
    (f, t, f, f, ko, f, f, oc, f) row,
    (ko, t, t, t, t, t, t, t, t) row,
    (f, t, t, t, t, oc, t, t, t) row,
    (f, f, ko, f, f, f, f, ko, a) row
  ) grid
  type start = <world:world; player:X.p; lvl:t; init:player_turn >

  type _ path =
    | []: start path
    | (::): ('a -> 'b) move * 'a path -> 'b path
end

module Lvl7
    (L1: clear with type 'a path = 'a Lvl1.path)
    (L2: clear with type 'a path = 'a Lvl2(L1).path)
    (L3: clear with type 'a path = 'a Lvl3(L1)(L2).path)
    (L4: clear with type 'a path = 'a Lvl4(L1)(L2)(L3).path)
    (L5: clear with type 'a path = 'a Lvl5(L1)(L2)(L3)(L4).path)
    (X: clear with type 'a path = 'a Lvl6(L1)(L2)(L3)(L4)(L5).path)
= struct

  type t
  open Short_cases
  type world = (
    (p, d, f, m, m, k, f, m, m) row,
    (m, m, f, m, m, m, f, oc, f) row,
    (m, m, f, m, m, m, f, m, k) row,
    (f, f, go, f, f, m, oc, m, m) row,
    (oc, m, m, m, f, f, f, f, oc) row,
    (k, f, go, m, m, m, f, m, og) row,
    (m, m, oc, m, m, m, ko, m, d) row,
    (r, m, f, m, e, m, f, m, d) row,
    (dr, f, f, m, s, og, f, m, g) row
  ) grid
  type start = <world:world; player:X.p; lvl:t; init:player_turn >

  type _ path =
    | []: start path
    | (::): ('a -> 'b) move * 'a path -> 'b path
end

module Lvl8
    (L1: clear with type 'a path = 'a Lvl1.path)
    (L2: clear with type 'a path = 'a Lvl2(L1).path)
    (L3: clear with type 'a path = 'a Lvl3(L1)(L2).path)
    (L4: clear with type 'a path = 'a Lvl4(L1)(L2)(L3).path)
    (L5: clear with type 'a path = 'a Lvl5(L1)(L2)(L3)(L4).path)
    (L6: clear with type 'a path = 'a Lvl6(L1)(L2)(L3)(L4)(L5).path)
    (X: clear with type 'a path = 'a Lvl7(L1)(L2)(L3)(L4)(L5)(L6).path)

= struct

  type t
  open Short_cases
  type world = (
    (f, m, m, m, f, oc, f, f, f) row,
    (f, f, f, f, f, m, m, m, f) row,
    (m, m, m, go, go, f, f, m, f) row,
    (m, m, m, f, f, f, f, m, f) row,
    (al, m, m, f, f, f, f, m, f) row,
    (m, m, m, f, m, m, m, m, f) row,
    (m, m, m, m, f, f, f, m, tr) row,
    (dr, m, m, m, f, m, f, m, f) row,
    (f, f, f, f, f, m, f, f, f ) row
  ) grid
  type start = <world:world; player:X.p; lvl:t; init:player_turn >

  type _ path =
    | []: start path
    | (::): ('a -> 'b) move * 'a path -> 'b path

  let win (_:<victory:yes> path) = exit 0
end
