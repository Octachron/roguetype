open Roguetype_lib

open Game.Lvl1

let start = []

let (=>) l x = x :: l

let v = start => R => R => R => D => D => R => D => D
  => L => L => L => L => L => L => L => L => G

(*
let w = v
        => R => R => D => R => R
        => U => U => U => U => U => U => U => U => G
*)
