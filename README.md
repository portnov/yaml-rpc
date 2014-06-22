YAML-RPC README
===============

Ilya V. Portnov <portnov84@rambler.ru>

The yaml-rpc package contains a small library to organize remote procedure call
(RPC) over TCP/IP network, using JSON as data serialization format.

RPC server should supply a set of "RPC methods", which are simply functions ::
a -> b -> ... -> IO c. Arguments must be of class ToJSON (defined in aeson package);
result must be of class FromJSON (defined in aeson package too). One can of
cause use GHC Generics mechanism to derive needed instances. yaml-rpc package
provides a (TemplateHaskell) function Network.YAML.TH.Server.makeAPI to
generate API description for server. Such API can be automatically written
to file in simple YAML format by using function Network.YAML.TH.Server.writeAPI.
It is possible to write different servers, which will use generated API
description and provide HTTP REST JSON services with that API. Currently there
is only one implementation using scotty package; it is provided by
yaml-rpc-scotty package. Please see yaml-rpc-scotty/Test/{Server.hs,
TestAPIImpl.hs} files for example usage.

RPC client calls that functions via HTTP REST JSON interface. So, it can be
used either from Haskell or from any other environment. For example,  it can be
easily used from JavaScript with JQuery or another framework.
For Haskell, yaml-rpc package provides a function Network.YAML.Caller.call to
call any method via HTTP REST JSON interface. Moreover, a (TemplateHaskell)
function Network.YAML.TH.Client.useAPI function will read API description from
file (in YAML format) and generate wrapper methods for calling respective
remote methods. Please see Test/Client.hs for example usage.

Currently, only one-connection-per-call mode is supported by Caller module.
There are plans to implement persistent connection support.

API description files can contain not only methods description, but also data
types description. Types description can be also automagically generated from
usual Haskell definitions and written to API description file. Then, when
reading that API description file, usual Haskell data type definitions are
generated.

Please see test.api file for example of API description format.

On the server side, usual workflow is as following:
* Write some number of API data types and functions to be exposed. 
* Call makeAPI function to generate API description.
* Call writeAPI function to write API description to file.
* Call generateDispatcher function on generated API to generate function
  dispatch :: Text -> Maybe (Value -> IO Value). This function will be used by
  any server implementation.
* Launch any server implementation and provide it by dispatch function.

On the client side, usual workflow is as following:
* Receive API description file from service provider.
* Call useAPI function on that file; it will generate data types definitions
  and wrapper functions to call all methods declared in file.
* Call generated wrappers.

Depends: ghc >= 7.6, yaml, template-haskell, th-lift, wreq, lens, scotty.

