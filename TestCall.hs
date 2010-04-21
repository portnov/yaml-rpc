
import Data.Object.Yaml
import Data.Convertible.Base

import YAML
import YAMLInstances 
import Caller

getService "test" = return ("127.0.0.1", 5000)
getService _ = fail "Unknown service"

p = Point 2.0 3.0

main = do
  srv <- getService "test"
  r <- call srv "double" p
  print (r :: Point)
