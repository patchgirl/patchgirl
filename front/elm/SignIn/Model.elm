module SignIn.Model exposing (..)

import Api.Generated as Client

type alias SignIn =
    { email: Client.CaseInsensitive
    , password: String
    }
