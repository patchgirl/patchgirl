{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Request where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           GHC.Generics
import           Servant

-- * DB

getOne :: Connection -> IO (Maybe String)
getOne connection = do
  mAgeAndNames <- (query connection rawQuery params) :: IO [Maybe (Int, String)]
  case mAgeAndNames of
    [Just(_, name)] -> do
      return $ Just name
    _ ->
      return Nothing
  where
    params = Only (2 :: Int)
    rawQuery = [sql|
              select id, text
              from request
              where id = ?
              |] :: Query

-- * MODEL

data Request
  = Request {
    requestId   :: Integer,
    requestText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Request
instance FromJSON Request

-- * Handler

getRequests :: Handler [Request]
getRequests = return [exampleRequest]

getRequestById :: Integer -> Handler Request
getRequestById = \case
  0 -> do
    connection <- liftIO getDBConnection
    --text <- liftIO (getOne connection)
    liftIO (getOne connection)
    --liftIO $ print text
    return exampleRequest
  _ -> throwError err404

-- * DB

exampleRequest :: Request
exampleRequest = Request 0 "example request"
