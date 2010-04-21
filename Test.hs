
import Data.Object.Yaml
import Data.Convertible.Base
import qualified Data.Map as M

import Dispatcher
import YAML
import YAMLInstances

double :: YamlObject -> IO YamlObject
double obj = do
  let (Point x y) = cs obj
  return $ cs $ Point (x*2) (y*2)

rules = mkRules [("double", double)]

main = do
  putStrLn "Listening..."
  dispatcher 5000 rules
  return ()
