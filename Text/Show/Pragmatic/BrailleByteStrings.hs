-- |
-- Module      : Text.Show.Pragmatic.BrailleByteStrings
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

module Text.Show.Pragmatic.BrailleByteStrings (
                 module Data.ByteString.Braille
               , module Text.Show.Pragmatic ) where

import Prelude hiding (Show(..), shows)
import Data.ByteString.Braille
import Text.Show.Pragmatic

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BC8

import Data.Char (isPrint)

instance Show (BS.ByteString) where
  showsPrec p bs
    | BC8.all isPrint bs
      = showsPrec p $ BC8.unpack bs
    | otherwise
      = showParen (p>9) $ ("bbits\""++) . (toBbits' bs++) . ('"':)
instance Show (BSL.ByteString) where
  showsPrec p bs
    | BC8.all isPrint bs'
      = showsPrec p $ BC8.unpack bs'
    | otherwise
      = showParen (p>9) $ ("bbits\""++) . (toBbits bs++) . ('"':)
   where bs' = BSL.toStrict bs
