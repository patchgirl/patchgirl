{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mailgun.App where

import           Control.Monad.Reader (MonadReader, ask)
import           Data.ByteString.UTF8 as BSU
import           Data.Functor         ((<&>))
import qualified Data.Text            as T
import qualified Mail.Hailgun         as Hailgun
import           PatchGirl


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

mkHailgunContext
  :: (MonadReader Config m)
  => m Hailgun.HailgunContext
mkHailgunContext = do
  mailgunConfig :: MailgunConfig <- ask <&> mailgun
  return $
  -- todo convert String -> Text
    Hailgun.HailgunContext { Hailgun.hailgunDomain = "" -- domain mailgunConfig
                           , Hailgun.hailgunApiKey = "" -- apiKey mailgunConfig
                           , Hailgun.hailgunProxy = Nothing
                           }


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


sendEmail :: Hailgun.HailgunContext -> Hailgun.HailgunMessage -> IO (Either Hailgun.HailgunErrorResponse Hailgun.HailgunSendResponse)
sendEmail hailgunContext message =
    Hailgun.sendEmail hailgunContext message
