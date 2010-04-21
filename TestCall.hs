
import Data.Object.Yaml
import Data.Convertible.Base

import YAML
import YAMLInstances 
import Caller

getServer "test" = return ("127.0.0.1", 5000)
getServer _ = fail "Unknown service"

p = Point 2.0 3.0

main = do
  r <- call getServer "test" "double" p
  print (r :: Point)
