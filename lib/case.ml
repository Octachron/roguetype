open Generic

module Tags = struct
  type door = Door
  type stair = Stair
  type 'a obstacle = Obstacle of 'a
  type 'a enemy = Enemy of 'a
  type 'a floor = Floor of 'a
  type free = Inventory.none floor
  type border = Border
end
type 'a eyable = < move:[`eye]; sticky:yes; tag:'a>
type 'a movable = < move:[`eye|`main]; sticky:yes; tag:'a>

type door = Tags.door eyable
type 'a floor = 'a Tags.floor movable
type stair = Tags.stair eyable
type 'a obstacle = 'a Tags.obstacle eyable
type 'a enemy = 'a Tags.enemy eyable
type border = < move: [`no ]; tag: Tags.border >
type free = Inventory.none floor
