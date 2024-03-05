module Things.Thing where

import Things.Sphere ( sphere )
import Things.Types ( Thing, WorldObject(Sphere), lambertian )

parseWorldObject :: WorldObject -> Thing
parseWorldObject (Sphere c r m) = sphere c r (lambertian m)
