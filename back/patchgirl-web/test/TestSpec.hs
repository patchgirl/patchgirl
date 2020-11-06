{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module TestSpec where

import           Servant
import           Servant.Client       (ClientM, client)
import           Test.Hspec

import           Helper.App
import           PatchGirl.Web.Api
import           PatchGirl.Web.Server
import           PatchGirl.Web.Test


-- * client


createUser :: NewUserTest -> ClientM UserTest
deleteUser :: Int -> ClientM ()
showUser :: Int -> ClientM UserTest
updateUser :: Int -> UpdateUserTest -> ClientM UserTest
updateUserRole :: Maybe Bool -> Int -> UserRole -> ClientM UserRole
listUsers :: ClientM [UserTest]
createProduct :: NewProductTest -> ClientM ProductTest
deleteProduct :: Int -> ClientM ()
showProduct :: Int -> ClientM ProductTest
updateProduct :: Int -> UpdateProductTest -> ClientM ProductTest
listProducts :: ClientM [ProductTest]
addToBasket :: Int -> AddToBasketTest -> ClientM ()
removeFromBasket :: Int -> RemoveFromBasketTest -> ClientM ()
showBasket :: Int -> ClientM BasketTest
_ :<|> _ :<|> _ :<|> _
  :<|> createUser
  :<|> deleteUser
  :<|> showUser
  :<|> updateUser
  :<|> updateUserRole
  :<|> listUsers
  :<|> createProduct
  :<|> deleteProduct
  :<|> showProduct
  :<|> updateProduct
  :<|> listProducts
  :<|> addToBasket
  :<|> removeFromBasket
  :<|> showBasket =
  client (Proxy :: Proxy TestApi)


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $ do


-- ** user


    describe "user test ressource" $ do
      it "creates a user" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test {} -> do
          UserTest{..} <- try clientEnv $ createUser NewUserTest { newUserTest_firstname = "John", newUserTest_lastname = "Doe"}
          userTest_firstname `shouldBe` "John"
          userTest_lastname `shouldBe` "Doe"

      it "deletes a user" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test {} -> do
          UserTest{..} <- try clientEnv $ createUser NewUserTest { newUserTest_firstname = "John", newUserTest_lastname = "Doe"}
          try clientEnv (deleteUser userTest_id) `shouldReturn` ()

      it "shows a user" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test {} -> do
          userTest <- try clientEnv $ createUser NewUserTest { newUserTest_firstname = "John", newUserTest_lastname = "Doe"}
          UserTest{..} <- try clientEnv $ showUser (userTest_id userTest)
          userTest_firstname `shouldBe` "John"
          userTest_lastname `shouldBe` "Doe"

      it "updates a user" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test {} -> do
          userTest <- try clientEnv $ createUser NewUserTest { newUserTest_firstname = "John", newUserTest_lastname = "Doe"}
          UserTest{..} <- try clientEnv $ updateUser (userTest_id userTest) $ UpdateUserTest { updateUserTest_firstname = "Jack"
                                                                                             , updateUserTest_lastname = "Terry"
                                                                                             }
          userTest_firstname `shouldBe` "Jack"
          userTest_lastname `shouldBe` "Terry"

      it "updates a role" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test {} -> do
          userTest <- try clientEnv $ createUser NewUserTest { newUserTest_firstname = "John", newUserTest_lastname = "Doe"}
          newUserRole <- try clientEnv $ updateUserRole (Just True) (userTest_id userTest) $ UserRole { userRole_role = "admin" }
          userRole_role newUserRole `shouldBe` "admin"

      it "list users" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test {} -> do
          userTest1 <- try clientEnv $ createUser NewUserTest { newUserTest_firstname = "John", newUserTest_lastname = "Doe"}
          userTest2 <- try clientEnv $ createUser NewUserTest { newUserTest_firstname = "Jack", newUserTest_lastname = "Raiden"}
          [user1, user2] <- try clientEnv listUsers
          user1 `shouldBe` userTest1
          user2 `shouldBe` userTest2


-- ** product


    describe "product test ressource" $ do
      it "creates a product" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test {} -> do
          ProductTest{..} <- try clientEnv $ createProduct NewProductTest { newProductTest_name = "ball", newProductTest_quantity = 2, newProductTest_price = 10 }
          productTest_name `shouldBe` "ball"
          productTest_quantity `shouldBe` 2
          productTest_price `shouldBe` 10

      it "deletes a product" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test {} -> do
          ProductTest{..} <- try clientEnv $ createProduct NewProductTest { newProductTest_name = "ball", newProductTest_quantity = 2, newProductTest_price = 10 }
          try clientEnv (deleteProduct productTest_id) `shouldReturn` ()

      it "shows a product" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test {} -> do
          productTest <- try clientEnv $ createProduct NewProductTest { newProductTest_name = "ball", newProductTest_quantity = 2, newProductTest_price = 10 }
          ProductTest{..} <- try clientEnv $ showProduct (productTest_id productTest)
          productTest_name `shouldBe` "ball"
          productTest_quantity `shouldBe` 2
          productTest_price `shouldBe` 10

      it "updates a product" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test {} -> do
          productTest <- try clientEnv $ createProduct NewProductTest { newProductTest_name = "ball", newProductTest_quantity = 2, newProductTest_price = 10 }
          ProductTest{..} <- try clientEnv $ updateProduct (productTest_id productTest) $ UpdateProductTest { updateProductTest_quantity = 3
                                                                                                            , updateProductTest_price = 20
                                                                                                            }
          productTest_quantity `shouldBe` 3
          productTest_price `shouldBe` 20

      it "list products" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test {} -> do
          productTest1 <- try clientEnv $ createProduct NewProductTest { newProductTest_name = "ball", newProductTest_quantity = 2, newProductTest_price = 10 }
          productTest2 <- try clientEnv $ createProduct NewProductTest { newProductTest_name = "basket", newProductTest_quantity = 3, newProductTest_price = 40 }
          [product1, product2] <- try clientEnv listProducts
          product1 `shouldBe` productTest1
          product2 `shouldBe` productTest2


-- ** basket


    describe "basket test ressource" $ do

      it "add to basket" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test {} -> do
          UserTest{..} <- try clientEnv $ createUser NewUserTest { newUserTest_firstname = "John", newUserTest_lastname = "Doe"}
          ProductTest{..} <- try clientEnv $ createProduct NewProductTest { newProductTest_name = "ball", newProductTest_quantity = 2, newProductTest_price = 10 }
          res <- try clientEnv $ addToBasket userTest_id $ AddToBasketTest { addToBasket_productId = productTest_id
                                                                           , addToBasket_quantity = 1
                                                                           }
          res `shouldBe` ()

      it "remove from basket" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test {} -> do
          UserTest{..} <- try clientEnv $ createUser NewUserTest { newUserTest_firstname = "John", newUserTest_lastname = "Doe"}
          ProductTest{..} <- try clientEnv $ createProduct NewProductTest { newProductTest_name = "ball", newProductTest_quantity = 2, newProductTest_price = 10 }
          try clientEnv $ addToBasket userTest_id $ AddToBasketTest { addToBasket_productId = productTest_id
                                                                    , addToBasket_quantity = 1
                                                                    }
          res <- try clientEnv $ removeFromBasket userTest_id $ RemoveFromBasketTest { removeFromBasketTest_productId = productTest_id
                                                                                     , removeFromBasketTest_productQuantity = 1
                                                                                     }
          res `shouldBe` ()

      it "shows basket" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test {} -> do
          UserTest{..} <- try clientEnv $ createUser NewUserTest { newUserTest_firstname = "John", newUserTest_lastname = "Doe"}
          ProductTest{..} <- try clientEnv $ createProduct NewProductTest { newProductTest_name = "ball", newProductTest_quantity = 2, newProductTest_price = 10 }
          try clientEnv $ addToBasket userTest_id $ AddToBasketTest { addToBasket_productId = productTest_id
                                                                    , addToBasket_quantity = 1
                                                                    }
          BasketTest {..} <- try clientEnv $ showBasket userTest_id
          basketTest_userId `shouldBe` userTest_id
          basketTest_purchases `shouldBe` [ PurchaseTest { purchaseTest_productId = productTest_id, purchaseTest_quantity = 1 } ]
