module Util.Flip exposing (flip, rotateArg)

flip : (a -> b -> c) -> b -> a -> c
flip f arg1 arg2 = f arg2 arg1

rotateArg : (a -> b -> c -> d) -> b -> c -> a -> d
rotateArg f argb argc arga =
    f arga argb argc
