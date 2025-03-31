
open Generic
type 'a t = private Monster

type 'a m = <health:'a> t
type kobold = Nat.one m
type goblin = Nat.two m
type orc = Nat.four m
type ogre = Nat.six m
type troll = Nat.eight m
type dragon = Nat.n256 m
