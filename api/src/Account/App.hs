{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Account.App where

import Account.Model
import Servant (Handler)

import Session.Model

meHandler :: Session -> Handler Account
meHandler (Session { accountId, accountEmail }) = do
  return $
    Account { _accountId = accountId
            , _accountEmail = accountEmail
            }
