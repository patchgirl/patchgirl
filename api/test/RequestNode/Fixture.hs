{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RequestNode.Fixture where

import Data.Aeson (Value)
import Data.Aeson.QQ
import Http
import RequestNode

-- * Folder

newRequestFolder1 :: NewRequestFolder
newRequestFolder1 =
  NewRequestFolder { newRequestFolderName = "1/"
                   , newRequestFolderParentNodeId = RequestCollectionId 1
                   }

createdRequestFolder1 :: CreatedRequestFolder
createdRequestFolder1 =
  CreatedRequestFolder { requestFolderId = 1
                       , requestFolderName = "1/"
                       , requestFolderCollectionId = Just 1
                       , requestFolderParentId = Nothing
                       }
-- * File

newRequestFile1 :: NewRequestFile
newRequestFile1 =
  NewRequestFile { newRequestFileName = "1"
                 , newRequestFileParentNodeId = RequestCollectionId 1
                 , newRequestFileMethod = Get
                 }

createdRequestFile1 :: CreatedRequestFile
createdRequestFile1 =
  CreatedRequestFile { createdRequestFileId = 1
                     , createdRequestFileName = "1"
                     , createdRequestFileCollectionId = Just 1
                     , createdRequestFileParentId = Nothing
                     }
