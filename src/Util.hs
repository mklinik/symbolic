module Util where

import Foundation

import qualified Data.Bits as B

import Types

wordToInt :: Word32 -> Int
wordToInt = fromIntegral . toInteger

wordToSignedInt :: Word32 -> Int
wordToSignedInt w =
  if isNegative w then
    (-(wordToInt (twosComplement w)))
  else
    wordToInt w

wordToBool :: Word32 -> Bool
wordToBool 0 = False
wordToBool _ = True

boolToWord :: Bool -> Word32
boolToWord True = 1
boolToWord False = 0

twosComplement :: Word32 -> Word32
twosComplement i = 1 + B.complement i

isNegative :: Word32 -> Bool
isNegative w = B.testBit w 31

valName :: Int -> String
valName i = "val_" <> (show i)

renderSym :: Sym -> String
renderSym (SAdd l r) = "(" <> renderSym l <> " + " <> renderSym r <> ")"
renderSym (SCon w) = show (wordToSignedInt w)
renderSym (SAny i) = valName i
renderSym (SEq l r) = renderSym l <> " = " <> renderSym r
renderSym (SNot c) = "~(" <> renderSym c <> ")"
renderSym (SAnd l r) = renderSym l <> " and " <> renderSym r
renderSym (SOr l r) = renderSym l <> " or " <> renderSym r
renderSym (SLt l r) = renderSym l <> " < " <> renderSym r
