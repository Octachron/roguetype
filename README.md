Roguetype is the first roguelike written in the OCaml type System.

In a world full of GADTs and functors, discover hidden paths through
mountains and forest, vanquish dragons, traverse the seven functor gates to
finally prove that the victory type is inhabited.

![Roguetype](https://github.com/user-attachments/assets/e5630e3a-f8b4-47b6-b4a8-dd369205f300)

## How to play with the marvelous interface Roguetype

Roguetype can be installed with

    opam pin add roguetype https://github.com/Octachron/roguetype.git
    
`roguetype` will then be launched in a toplevel with the `Roguetype_lib`
library already loaded.

Refer to the `examples/start.ml` example as a simple path through the first
level of Roguetype. Winning `Roguetype` merely requires to call the `win`
defined inside the `Lvl8` functor of the `Game` module.

It may be easier to use the library directly in your favorite editor
to explore the typing rules of `roguetype`.
