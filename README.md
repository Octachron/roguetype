Roguetype is the first roguelike written in the OCaml type System.

In a world full of GADTs and type constraints, through mountains and forest,
discover hidden paths, vanquish dragons, in order to traverse
the seven functor gate and prove that the victory type is inhabited.

## How to play with the marvelous interface Roguetype

Refer to the `examples/start.ml` example as a simple path through
the first level of Roguetype. Winning `Roguetype` merely requires
to call the `win` defined inside the `Lvl8` functor of the `Game`
module[^1].

[^1]: Theoretically, type checking time of levels should be finite,
this affirmation has however not yet be confirmed empirically however.
