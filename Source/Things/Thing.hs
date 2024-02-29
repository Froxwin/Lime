module Things.Thing where

import Things.Sphere ( sphere )
import Things.Types ( Thing, WorldObject(Sphere) )

parseWorldObject :: WorldObject -> Thing
parseWorldObject (Sphere c r) = sphere c r
