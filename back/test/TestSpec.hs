{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators             #-}

module TestSpec where

import           Servant
import           Servant.Client (ClientM, client)
import           Test.Hspec

import           App
import           Helper.App
import           Test


-- * client


createUser :: NewUserTest -> ClientM UserTest
deleteUser :: Int -> ClientM ()
showUser :: Int -> ClientM UserTest
updateUser :: Int -> UpdateUserTest -> ClientM UserTest
listUsers :: ClientM [UserTest]
_ :<|> _
  :<|> createUser
  :<|> deleteUser
  :<|> showUser
  :<|> updateUser
  :<|> listUsers =
  client (Proxy :: Proxy (TestApi))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $ do


-- ** handles user resource


    fdescribe "user test ressource" $ do
      it "creates a user" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test {} -> do
          UserTest{..} <- try clientEnv $ createUser NewUserTest { newUserFirstname = "John", newUserLastname = "Doe"}
          userTest_firstname `shouldBe` "John"
          userTest_lastname `shouldBe` "Doe"

      it "deletes a user" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test {} -> do
          UserTest{..} <- try clientEnv $ createUser NewUserTest { newUserFirstname = "John", newUserLastname = "Doe"}
          try clientEnv (deleteUser userTest_id) `shouldReturn` ()

      it "shows a user" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test {} -> do
          userTest <- try clientEnv $ createUser NewUserTest { newUserFirstname = "John", newUserLastname = "Doe"}
          UserTest{..} <- try clientEnv $ showUser (userTest_id userTest)
          userTest_firstname `shouldBe` "John"
          userTest_lastname `shouldBe` "Doe"

      it "updates a user" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test {} -> do
          userTest <- try clientEnv $ createUser NewUserTest { newUserFirstname = "John", newUserLastname = "Doe"}
          UserTest{..} <- try clientEnv $ updateUser (userTest_id userTest) $ UpdateUserTest { updateUserTest_firstname = "Jack"
                                                                                             , updateUserTest_lastname = "Terry"
                                                                                             }
          userTest_firstname `shouldBe` "Jack"
          userTest_lastname `shouldBe` "Terry"

      it "list users" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test {} -> do
          userTest1 <- try clientEnv $ createUser NewUserTest { newUserFirstname = "John", newUserLastname = "Doe"}
          userTest2 <- try clientEnv $ createUser NewUserTest { newUserFirstname = "Jack", newUserLastname = "Raiden"}
          [user1, user2] <- try clientEnv listUsers
          user1 `shouldBe` userTest1
          user2 `shouldBe` userTest2
