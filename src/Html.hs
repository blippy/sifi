{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Html where

import Data.ByteString.Char8 (unpack)
import Data.FileEmbed

--import Config
import Utils

htmlDoc = $(embedFile "resources/sifi.htm")


saveHtml = do
  dst <- outFile "sifi.htm"
  let str = unpack htmlDoc -- :: B.ByteString
  writeFile dst str
