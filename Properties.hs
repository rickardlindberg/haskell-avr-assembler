import Test.QuickCheck

import Instructions

prop_encodeDecode :: Instruction -> Bool
prop_encodeDecode x =
    let (res, []) = decode (encode x)
    in  res == x

main = do
    quickCheck prop_encodeDecode
