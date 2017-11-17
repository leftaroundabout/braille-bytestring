-- |
-- Module      : Data.ByteString.Braille
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

module Data.ByteString.Braille (bbits, toBbits, toBbits') where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word

import Data.String (IsString(..))
import qualified Text.Show.Pragmatic as PS

import Data.CallStack


bbits :: (HasCallStack, IsString bs) => String -> bs
bbits = fromString . map fromBrailleC
 where fromBrailleC c
        | c >= '⠀' && c <= '⣿'
                     = toEnum $ fromEnum c - fromEnum '⠀'
        | otherwise  = error
            $ "`bbits` can only take Braille characters, i.e '⠀'..'⡵'..'⣿'. Cannot handle '"
                  ++[c]++"'."

toBbits :: BSL.ByteString -> String
toBbits = map toBrailleC . BSL.unpack

toBbits' :: BS.ByteString -> String
toBbits' = map toBrailleC . BS.unpack

toBrailleC :: Word8 -> Char
toBrailleC = toEnum . (+fromEnum '⠀') . fromIntegral
