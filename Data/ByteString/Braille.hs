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
import qualified Data.Vector.Unboxed as UArr

import Data.List (sort)

import Data.String (IsString(..))
import qualified Text.Show.Pragmatic as PS

import Data.CallStack


bbits :: (HasCallStack, IsString bs) => String -> bs
bbits = fromString . map (toEnum . fromIntegral . fromBrailleC)
 where fromBrailleC c
        | c >= '⠀' && c <= '⣿'
                     = UArr.unsafeIndex invPalette $ fromEnum c - fromEnum '⠀'
        | otherwise  = error
            $ "`bbits` can only take Braille characters, i.e '⠀'..'⡵'..'⣿'. Cannot handle '"
                  ++[c]++"'."

toBbits :: BSL.ByteString -> String
toBbits = map toBrailleC . BSL.unpack

toBbits' :: BS.ByteString -> String
toBbits' = map toBrailleC . BS.unpack

toBrailleC :: Word8 -> Char
toBrailleC = UArr.unsafeIndex braillePalette . fromIntegral

braillePalette :: UArr.Vector Char
braillePalette = UArr.fromList $ "⠀⢀⠠⢠⠐⢐⠰⢰⠈⢈⠨⢨⠘⢘⠸⢸"
                              ++ "⡀⣀⡠⣠⡐⣐⡰⣰⡈⣈⡨⣨⡘⣘⡸⣸"
                              ++ "⠄⢄⠤⢤⠔⢔⠴⢴⠌⢌⠬⢬⠜⢜⠼⢼"
                              ++ "⡄⣄⡤⣤⡔⣔⡴⣴⡌⣌⡬⣬⡜⣜⡼⣼"
                              ++ "⠂⢂⠢⢢⠒⢒⠲⢲⠊⢊⠪⢪⠚⢚⠺⢺"
                              ++ "⡂⣂⡢⣢⡒⣒⡲⣲⡊⣊⡪⣪⡚⣚⡺⣺"
                              ++ "⠆⢆⠦⢦⠖⢖⠶⢶⠎⢎⠮⢮⠞⢞⠾⢾"
                              ++ "⡆⣆⡦⣦⡖⣖⡶⣶⡎⣎⡮⣮⡞⣞⡾⣾"
                              ++ "⠁⢁⠡⢡⠑⢑⠱⢱⠉⢉⠩⢩⠙⢙⠹⢹"
                              ++ "⡁⣁⡡⣡⡑⣑⡱⣱⡉⣉⡩⣩⡙⣙⡹⣹"
                              ++ "⠅⢅⠥⢥⠕⢕⠵⢵⠍⢍⠭⢭⠝⢝⠽⢽"
                              ++ "⡅⣅⡥⣥⡕⣕⡵⣵⡍⣍⡭⣭⡝⣝⡽⣽"
                              ++ "⠃⢃⠣⢣⠓⢓⠳⢳⠋⢋⠫⢫⠛⢛⠻⢻"
                              ++ "⡃⣃⡣⣣⡓⣓⡳⣳⡋⣋⡫⣫⡛⣛⡻⣻"
                              ++ "⠇⢇⠧⢧⠗⢗⠷⢷⠏⢏⠯⢯⠟⢟⠿⢿"
                              ++ "⡇⣇⡧⣧⡗⣗⡷⣷⡏⣏⡯⣯⡟⣟⡿⣿"
{-# NOINLINE braillePalette #-}

invPalette :: UArr.Vector Word8
invPalette
   = UArr.fromList . map snd . sort $ zip (UArr.toList braillePalette) [0..]
{-# NOINLINE invPalette #-}
