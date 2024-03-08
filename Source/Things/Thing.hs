module Things.Thing where

import           Things.Sphere                  ( sphere )
import           Things.Types

import           Materials                      ( lambertian, metal )

parseWorldObject :: WorldObject -> Thing
parseWorldObject (Sphere c r m q)
  | m == Lam = sphere c r (lambertian q)
  | m == Metal = sphere c r (metal q)
