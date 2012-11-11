import Test.QuickCheck

import Instructions

prop_encodeDecode :: Instruction -> Bool
prop_encodeDecode x = normalize x' == normalize x
    where
        (x', []) = decode (encode x)
        normalize (BRBC 0 k) = BRCC k
        normalize x          = x

main = do
    quickCheck prop_encodeDecode
