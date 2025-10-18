let log = Console.log

type status = Tracking | Watching | Completed

let m = Map.make()

m->Map.set(Watching, 1)
m->Map.set(Tracking, 2)
m->Map.set(Completed, 2)

m->log

type coord = Coord(int, int)

let n = Map.make()

let twotwo = Coord(2,2)
n->Map.set(Coord(1,1), 2)
n->Map.set(twotwo, 4)

n->log

n->Map.get(twotwo)->log
