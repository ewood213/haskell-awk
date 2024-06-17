module Value where

import Prelude hiding (not, and)
import Text.Read (readMaybe)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Fixed
import Data.Semigroup

data Value = StringVal ByteString | FloatVal Float deriving (Show, Eq)

defaultBinOp :: (Float -> Float -> Float) -> Value -> Value -> Value
defaultBinOp op v1 v2 = FloatVal $ toFloat v1 `op` toFloat v2

defaultCompOp :: (Float -> Float -> Bool) -> Value -> Value -> Value
defaultCompOp op v1 v2 = fromBool $ toFloat v1 `op` toFloat v2

plus :: Value -> Value -> Value
plus = defaultBinOp (+)

times :: Value -> Value -> Value
times = defaultBinOp (*)

minus :: Value -> Value -> Value
minus = defaultBinOp (-)

divide :: Value -> Value -> Value
divide = defaultBinOp (/)

mod :: Value -> Value -> Value
mod = defaultBinOp (mod')

equal :: Value -> Value -> Value
equal (StringVal s) (FloatVal f) = case readMaybe $ BS.unpack s of
                                    Just fs -> fromBool $ fs == f
                                    otherwise -> fromBool False
equal f@(FloatVal _) s@(StringVal _) = equal s f
equal a b = fromBool $ a == b

nequal :: Value -> Value -> Value
nequal a b = not $ equal a b

lessThan :: Value -> Value -> Value
lessThan (StringVal s1) (StringVal s2) = fromBool $ s1 < s2
lessThan a b = defaultCompOp (<) a b

greaterThan :: Value -> Value -> Value
greaterThan a b = not (lessThan a b) `and` not (equal a b)

lessEqual :: Value -> Value -> Value
lessEqual a b = not $ greaterThan a b

greaterEqual :: Value -> Value -> Value
greaterEqual a b = not $ lessThan a b

not :: Value -> Value
not (StringVal _) = fromBool False
not (FloatVal f) = fromBool $ f == 0

toBool :: Value -> Bool
toBool (StringVal _) = False
toBool (FloatVal f) = f /= 0

fromBool :: Bool -> Value
fromBool False = FloatVal 0
fromBool True = FloatVal 1

and :: Value -> Value -> Value
and a b = fromBool $ toBool a && toBool b

or :: Value -> Value -> Value
or a b = fromBool $ toBool a || toBool b

negate :: Value -> Value
negate (FloatVal f) = FloatVal (-f)
negate a = a

showVal :: Value -> ByteString
showVal (StringVal s) = s
showVal (FloatVal f) =
    if f == fromIntegral (truncate f)
        then BS.pack $ show $ truncate f
        else BS.pack $ show f

toFloat :: Value -> Float
toFloat (StringVal s) = case readMaybe $ BS.unpack s of
                            Just f -> f
                            otherwise -> 0
toFloat (FloatVal f) = f
