type yes = True
type no = False

module Nat = struct
  type z = private Tag_z
  type o = private Tag_one
  type one = o -> z
  type two = o -> one
  type three = o -> two
  type four = o -> three
  type six = o -> o -> four

  type 'a s = o -> 'a
  type 'a p4 = 'a s s s s
  type 'a p256 = 'a p4 p4 p4 p4

  type n256 = z p256
end
