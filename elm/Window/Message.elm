module Window.Message exposing (..)

import Builder.Message as Builder
import Tree.Message as Tree
import Postman.Message as Postman
import Env.Message as Env
import EnvNav.Message as EnvNav
import Runner.Message as Runner
import Tab.Message as Tab


type Msg
  = TreeMsg Tree.Msg
  | BuilderMsg Builder.Msg
  | PostmanMsg Postman.Msg
  | EnvMsg Env.Msg
  | EnvNavMsg EnvNav.Msg
  | RunnerMsg Runner.Msg
  | TabMsg Tab.Msg
