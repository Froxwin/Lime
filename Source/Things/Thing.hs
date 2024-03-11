module Things.Thing where

import           Things.Sphere
import           Things.Types
import           Materials

parseWorldObject :: WorldObject -> Thing
parseWorldObject (Sphere c r (Lambertian q)) = sphere c r (lambertian q)
parseWorldObject (Sphere c r (Metal q f   )) = sphere c r (metal q f)
parseWorldObject (Sphere c r (Dielectric i)) = sphere c r (dielectric i)
