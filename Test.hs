
import Data.Object.Yaml
import Data.Convertible.Base

import Server
import YAML
import YAMLInstances

worker :: YamlObject -> IO YamlObject
worker obj = do
  let (Point x y) = cs obj
  return $ cs $ Point (x*2) (y*2)

main = do
  print "Listening..."
  server 5000 worker
  return ()
