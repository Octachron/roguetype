type door = Door
type gate = Gate
type altar = Altar
type forest = Forest
type mountain = Mountain
type border = mountain

type 'a floor = Floor of 'a
type free = Inventory.none floor
type key = Inventory.key floor
type mithril_sword = Inventory.mithril_sword floor
type cristal_sword = Inventory.cristal_sword floor
type axe = Inventory.axe floor
type potion = Inventory.potion floor
type elixir = Inventory.elixir floor
type ring_of_annihilation = Inventory.ring_of_annihilation floor

type 'a monster = 'a Monster.t
type kobold = Monster.kobold
type goblin = Monster.goblin
type orc = Monster.orc
type ogre = Monster.ogre
type dragon = Monster.dragon
type troll = Monster.troll
