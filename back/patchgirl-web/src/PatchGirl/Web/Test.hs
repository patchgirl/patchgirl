{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PatchGirl.Web.Test where

import qualified Control.Monad                        as Monad
import qualified Control.Monad.Except                 as Except
import qualified Control.Monad.IO.Class               as IO
import qualified Control.Monad.Reader                 as Reader
import           Data.Aeson                           (Value)
import qualified Data.Aeson                           as Aeson
import           Data.Aeson.Types                     (FromJSON (..),
                                                       ToJSON (..), parseEither)
import qualified Data.ByteString.Char8                as B
import           Data.Function                        ((&))
import           Data.Functor                         ((<&>))
import qualified Data.List                            as List
import qualified Data.Maybe                           as Maybe
import           Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Simple           as PG
import           Database.PostgreSQL.Simple.FromField hiding (name)
import qualified Database.PostgreSQL.Simple.FromField as PG
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics                         (Generic)
import qualified GHC.Int                              as Int
import           Servant

import           PatchGirl.Web.DB
import           PatchGirl.Web.Internal.Env


-- * model


-- ** signin


data SignInTest = SignInTest
    { signInTest_email    :: String
    , signInTest_password :: String
    }
    deriving (Eq, Show, Generic)

instance Aeson.ToJSON SignInTest where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("signInTest_" :: String) }

instance Aeson.FromJSON SignInTest where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("signInTest_" :: String) }


-- ** product


data ProductTest = ProductTest
    { productTest_id       :: Int
    , productTest_name     :: String
    , productTest_quantity :: Int
    , productTest_price    :: Int
    }
    deriving (Eq, Show, Read, Generic, PG.FromRow)

instance Aeson.ToJSON ProductTest where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("productTest_" :: String) }

instance Aeson.FromJSON ProductTest where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("productTest_" :: String) }


-- *** new product


data NewProductTest = NewProductTest
    { newProductTest_name     :: String
    , newProductTest_quantity :: Int
    , newProductTest_price    :: Int
    }
    deriving (Eq, Show, Read, Generic, PG.FromRow, PG.ToRow)

instance Aeson.ToJSON NewProductTest where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("newProductTest_" :: String) }

instance Aeson.FromJSON NewProductTest where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("newProductTest_" :: String) }


-- *** update product


data UpdateProductTest = UpdateProductTest
    { updateProductTest_quantity :: Int
    , updateProductTest_price    :: Int
    }
    deriving (Eq, Show, Read, Generic)

instance Aeson.ToJSON UpdateProductTest where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("updateProductTest_" :: String) }

instance Aeson.FromJSON UpdateProductTest where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("updateProductTest_" :: String) }


-- ** basket


data BasketTest = BasketTest
    { basketTest_userId    :: Int
    , basketTest_purchases :: [PurchaseTest]
    }
    deriving (Eq, Show, Read, Generic)

instance Aeson.ToJSON BasketTest where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("basketTest_" :: String) }

instance Aeson.FromJSON BasketTest where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("basketTest_" :: String) }


-- *** purchase


data PurchaseTest = PurchaseTest
    { purchaseTest_productId :: Int
    , purchaseTest_quantity  :: Int
    }
    deriving (Eq, Show, Read, Generic)

instance PG.FromField [PurchaseTest] where
  fromField field mdata =
    (fromField field mdata :: Conversion (Maybe Value)) >>= \case
      Nothing ->
        return []
      Just value -> do
        let errorOrPgNodes = parseEither parseJSON value :: Either String [PurchaseTest]
        either
          (returnError ConversionFailed field)
          return
          errorOrPgNodes

instance Aeson.ToJSON PurchaseTest where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("purchaseTest_" :: String) }

instance Aeson.FromJSON PurchaseTest where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("purchaseTest_" :: String) }


-- *** add to basket


data AddToBasketTest = AddToBasketTest
    { addToBasket_productId :: Int
    , addToBasket_quantity  :: Int
    }
    deriving (Eq, Show, Read, Generic)

instance Aeson.ToJSON AddToBasketTest where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("addToBasket_" :: String) }

instance Aeson.FromJSON AddToBasketTest where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("addToBasket_" :: String) }


-- *** remove from basket


data RemoveFromBasketTest = RemoveFromBasketTest
    { removeFromBasketTest_productId       :: Int
    , removeFromBasketTest_productQuantity :: Int
    }
    deriving (Eq, Show, Read, Generic)

instance Aeson.ToJSON RemoveFromBasketTest where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("removeFromBasketTest_" :: String) }

instance Aeson.FromJSON RemoveFromBasketTest where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("removeFromBasketTest_" :: String) }


-- ** user


data UserTest = UserTest
    { userTest_id        :: Int
    , userTest_firstname :: String
    , userTest_lastname  :: String
    }
    deriving (Eq, Show, Read, Generic, PG.FromRow)

instance Aeson.ToJSON UserTest where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("userTest_" :: String) }

instance Aeson.FromJSON UserTest where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("userTest_" :: String) }


-- *** new user


data NewUserTest = NewUserTest
    { newUserTest_firstname :: String
    , newUserTest_lastname  :: String
    }
    deriving (Eq, Show, Read, Generic, PG.ToRow)

instance Aeson.ToJSON NewUserTest where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("newUserTest_" :: String) }

instance Aeson.FromJSON NewUserTest where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("newUserTest_" :: String) }


-- *** update user


data UpdateUserTest = UpdateUserTest
    { updateUserTest_firstname :: String
    , updateUserTest_lastname  :: String
    }
    deriving (Eq, Show, Read, Generic)

instance Aeson.ToJSON UpdateUserTest where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("updateUserTest_" :: String) }

instance Aeson.FromJSON UpdateUserTest where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("updateUserTest_" :: String) }


-- *** user role


newtype UserRole
  = UserRole { userRole_role :: String }
  deriving (Eq, Show, Read, Generic)

instance PG.FromField UserRole where
   fromField f mdata =
     case B.unpack `fmap` mdata of
       Nothing   -> PG.returnError PG.UnexpectedNull f ""
       Just role -> return UserRole { userRole_role = role }

instance Aeson.ToJSON UserRole where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("userRole_" :: String) }

instance Aeson.FromJSON UserRole where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("userRole_" :: String) }


-- * handler


-- ** sign in


signInHandler :: IO.MonadIO m => SignInTest -> m (Headers '[ Header "Set-Cookie" String] String)
signInHandler SignInTest{..} = do
  return $ case (signInTest_email, signInTest_password) of
    ("admin", "admin") -> addHeader adminCookie "ok, you're signed in"
    _                  -> noHeader "wrong password"
    where
      adminCookie :: String
      adminCookie =
        [ ("Admin", show True)
        , ( "Path", "/" )
        ] <&> makeHeader  & List.intercalate ";"

      makeHeader :: (String, String) -> String
      makeHeader (key, value) = key <> "=" <> value


-- ** check super secret


checkSuperSecretHandler :: IO.MonadIO m => Maybe String -> m String
checkSuperSecretHandler = return . \case
  Just "Admin=True" ->
    "this is the super secret: coucou"

  _ ->
    "you need to be admin to see the super secret"


-- ** product


-- *** create product


createProductHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => NewProductTest
  -> m ProductTest
createProductHandler newProduct = do
  connection <- getDBConnection
  IO.liftIO $ insertNewProductSql newProduct connection

insertNewProductSql :: NewProductTest -> PG.Connection -> IO ProductTest
insertNewProductSql newProduct connection = do
  [product] <- PG.query connection rawQuery newProduct
  return product
  where
    rawQuery =
      [sql|
          INSERT INTO product_test (name, quantity, price)
          VALUES (?, ?, ?)
          RETURNING id, name, quantity, price
          |]



-- *** delete product


deleteProductHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => Int
  -> m ()
deleteProductHandler productId = do
  connection <- getDBConnection
  IO.liftIO . Monad.void $ deleteProductTestSql productId connection

deleteProductTestSql :: Int -> PG.Connection -> IO Int.Int64
deleteProductTestSql productId connection = do
  PG.execute connection rawQuery (PG.Only productId)
  where
    rawQuery =
      [sql|
          UPDATE product_test
          SET deleted_at = NOW()
          WHERE id = ?
          |]


-- *** show product


showProductHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Int
  -> m ProductTest
showProductHandler productId = do
  connection <- getDBConnection
  mProduct <- IO.liftIO $ showProductTestSql productId connection
  case mProduct of
    Nothing -> Servant.throwError Servant.err404
    Just product ->
      return product

showProductTestSql :: Int -> PG.Connection -> IO (Maybe ProductTest)
showProductTestSql productId connection = do
  PG.query connection rawQuery (PG.Only productId) <&> Maybe.listToMaybe
  where
    rawQuery =
      [sql|
          SELECT id, name, quantity, price
          FROM product_test
          WHERE id = ?
          AND deleted_at IS NULL
          |]


-- *** update product

updateProductHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => Int
  -> UpdateProductTest
  -> m ProductTest
updateProductHandler productId updateProduct = do
  connection <- getDBConnection
  IO.liftIO $ updateProductTestSql productId updateProduct connection

updateProductTestSql :: Int -> UpdateProductTest -> PG.Connection -> IO ProductTest
updateProductTestSql productId UpdateProductTest{..} connection = do
  [product] <- PG.query connection rawQuery ( updateProductTest_quantity
                                            , updateProductTest_price
                                            , productId
                                            )
  return product
  where
    rawQuery =
      [sql|
          UPDATE product_test
          SET quantity = ?, price = ?
          WHERE id = ?
          RETURNING id, name, quantity, price
          |]


-- *** list products


listProductHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => m [ProductTest]
listProductHandler = do
  connection <- getDBConnection
  IO.liftIO $ listProductTestSql connection

listProductTestSql :: PG.Connection -> IO [ProductTest]
listProductTestSql connection = do
  PG.query_ connection rawQuery
  where
    rawQuery =
      [sql|
          SELECT id, name, quantity, price
          FROM product_test
          WHERE deleted_at IS NULL
          |]


-- ** user
-- *** create user


createUserHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => NewUserTest
  -> m UserTest
createUserHandler newUser = do
  connection <- getDBConnection
  IO.liftIO $ insertNewUserTestSql newUser connection

insertNewUserTestSql :: NewUserTest -> PG.Connection -> IO UserTest
insertNewUserTestSql newUser connection = do
  [user] <- PG.query connection rawQuery newUser
  return user
  where
    rawQuery =
      [sql|
          INSERT INTO user_test (firstname, lastname, role)
          VALUES (?, ?, 'normal')
          RETURNING id, firstname, lastname
          |]


-- *** delete user


deleteUserHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => Int
  -> m ()
deleteUserHandler userId = do
  connection <- getDBConnection
  IO.liftIO . Monad.void $ deleteUserTestSql userId connection

deleteUserTestSql :: Int -> PG.Connection -> IO Int.Int64
deleteUserTestSql userId connection = do
  PG.execute connection rawQuery (PG.Only userId)
  where
    rawQuery =
      [sql|
          UPDATE user_test
          SET deleted_at = NOW()
          WHERE id = ?
          |]


-- *** show user


showUserHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => Int
  -> m UserTest
showUserHandler userId = do
  connection <- getDBConnection
  IO.liftIO $ showUserTestSql userId connection

showUserTestSql :: Int -> PG.Connection -> IO UserTest
showUserTestSql userId connection = do
  [user] <- PG.query connection rawQuery (PG.Only userId)
  return user
  where
    rawQuery =
      [sql|
          SELECT id, firstname, lastname
          FROM user_test
          WHERE id = ?
          |]


-- *** update user


updateUserHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => Int
  -> UpdateUserTest
  -> m UserTest
updateUserHandler userId updateUser = do
  connection <- getDBConnection
  IO.liftIO $ updateUserTestSql userId updateUser connection

updateUserTestSql :: Int -> UpdateUserTest -> PG.Connection -> IO UserTest
updateUserTestSql userId UpdateUserTest{..} connection = do
  [user] <- PG.query connection rawQuery ( updateUserTest_firstname
                                         , updateUserTest_lastname
                                         , userId
                                         )
  return user
  where
    rawQuery =
      [sql|
          UPDATE user_test
          SET firstname = ?, lastname = ?
          WHERE id = ?
          RETURNING id, firstname, lastname
          |]


-- *** update role


updateRoleHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Maybe Bool
  -> Int
  -> UserRole
  -> m UserRole
updateRoleHandler mIsAdmin userId updateUserTestRole =
  case mIsAdmin of
    Just True -> do
      connection <- getDBConnection
      IO.liftIO $ updateUserTestRoleSql userId updateUserTestRole connection

    _ ->
      Servant.throwError Servant.err404

updateUserTestRoleSql :: Int -> UserRole -> PG.Connection -> IO UserRole
updateUserTestRoleSql userId UserRole{..} connection = do
  [ PG.Only role ] <- PG.query connection rawQuery ( userRole_role
                                                   , userId
                                                   )
  return role
  where
    rawQuery =
      [sql|
          UPDATE user_test
          SET role = ?
          WHERE id = ?
          RETURNING role
          |]


-- *** list users


listUserHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => m [UserTest]
listUserHandler = do
  connection <- getDBConnection
  IO.liftIO $ listUserTestSql connection

listUserTestSql :: PG.Connection -> IO [UserTest]
listUserTestSql connection = do
  PG.query_ connection rawQuery
  where
    rawQuery =
      [sql|
          SELECT id, firstname, lastname
          FROM user_test
          WHERE deleted_at IS NULL
          |]


-- ** basket


-- *** add to basket


addToBasketHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => Int
  -> AddToBasketTest
  -> m ()
addToBasketHandler userId addToBasket = do
  connection <- getDBConnection
  IO.liftIO $ addToBasketSql userId addToBasket connection

addToBasketSql :: Int -> AddToBasketTest -> PG.Connection -> IO ()
addToBasketSql userId AddToBasketTest{..} connection = do
  _ <- PG.execute connection rawQuery ( userId
                                      , addToBasket_productId
                                      , addToBasket_quantity
                                      )
  return ()
  where
    rawQuery =
      [sql|
          INSERT INTO basket_product_test (user_id, product_id, quantity)
          VALUES (?, ?, ?)
          |]


-- *** remove from basket


removeFromBasketHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => Int
  -> RemoveFromBasketTest
  -> m ()
removeFromBasketHandler userId removeFromBasket = do
  connection <- getDBConnection
  IO.liftIO . Monad.void $ removeFromBasketSql userId removeFromBasket connection

removeFromBasketSql :: Int -> RemoveFromBasketTest -> PG.Connection -> IO Int.Int64
removeFromBasketSql userId RemoveFromBasketTest{..} connection = do
  PG.execute connection rawQuery ( removeFromBasketTest_productQuantity
                                 , userId
                                 , removeFromBasketTest_productId
                                 )
  where
    rawQuery =
      [sql|
          UPDATE basket_product_test
          SET quantity = quantity - ?
          WHERE user_id = ?
          AND product_id = ?
          |]


-- *** show basket


showBasketHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Int
  -> m BasketTest
showBasketHandler userId = do
  connection <- getDBConnection
  mBasket <- IO.liftIO $ showBasketTestSql userId connection
  case mBasket of
    Nothing -> Servant.throwError Servant.err404
    Just basket ->
      return basket

showBasketTestSql :: Int -> PG.Connection -> IO (Maybe BasketTest)
showBasketTestSql userId connection = do
  mUserIdToPurchase :: Maybe (Int, [PurchaseTest])  <- PG.query connection rawQuery (PG.Only userId) <&> Maybe.listToMaybe
  return $ mUserIdToPurchase <&> \(_, purchases) ->
    BasketTest { basketTest_userId = userId
               , basketTest_purchases = purchases
               }
  where
    rawQuery =
      [sql|
          SELECT user_id, json_agg( json_build_object('productId', product_id, 'quantity', quantity) )
          FROM basket_product_test
          WHERE user_id = ?
          GROUP BY user_id;
          |]


-- ** other


deleteNoContentHandler :: (Reader.MonadReader Env m) => m NoContent
deleteNoContentHandler =
  return NoContent

getStatusCodeHandler :: (Except.MonadError ServerError m) => Int -> m ()
getStatusCodeHandler = \case
  200 -> return ()
  404 -> Servant.throwError Servant.err404
  _ -> Servant.throwError Servant.err404
