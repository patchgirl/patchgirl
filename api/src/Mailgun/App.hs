{-# LANGUAGE NamedFieldPuns #-}

module Mailgun.App where

import           Data.ByteString.UTF8 as BSU
import qualified Data.Text            as T
import qualified Mail.Hailgun         as Hailgun


-- * model


data EmailCtx =
  EmailCtx { _emailDomain :: String
           , _emailApiKey :: String
           }

data Email =
  Email { _emailSubject         :: String
         , _emailMessageContent :: String
         , _emailRecipients     :: [String]
         }


-- * util


mkHailgunMessage :: Email -> Either Hailgun.HailgunErrorMessage Hailgun.HailgunMessage
mkHailgunMessage (Email { _emailSubject, _emailMessageContent, _emailRecipients }) =
  let
    hailgunMessageSubject =
      T.pack _emailSubject

    hailgunMessageContent =
      Hailgun.TextOnly { Hailgun.textContent = BSU.fromString _emailMessageContent }

    hailgunAuthor =
      BSU.fromString "mailgun@sandbox4818ed69121942ec8a93c28bef0e0edd.mailgun.org"

    hailgunRecipients =
      Hailgun.MessageRecipients { Hailgun.recipientsTo = BSU.fromString <$> _emailRecipients
                                , Hailgun.recipientsCC = []
                                , Hailgun.recipientsBCC = []
                                }

    hailgunAttachments =
      []
  in
    Hailgun.hailgunMessage hailgunMessageSubject hailgunMessageContent hailgunAuthor hailgunRecipients hailgunAttachments


-- * send email


sendEmail :: EmailCtx -> Hailgun.HailgunMessage -> IO (Either Hailgun.HailgunErrorResponse Hailgun.HailgunSendResponse)
sendEmail (EmailCtx { _emailDomain, _emailApiKey }) message =
  let
    hailgunContext =
      Hailgun.HailgunContext { Hailgun.hailgunDomain = _emailDomain
                             , Hailgun.hailgunApiKey = _emailApiKey
                             , Hailgun.hailgunProxy = Nothing
                             }


  in
    Hailgun.sendEmail hailgunContext message
