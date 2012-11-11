import Test.QuickCheck

import Instructions

prop_encodeDecode :: Instruction -> Bool
prop_encodeDecode x = normalize x' == normalize x
    where
        (x', []) = decode (encode x)

        normalize (BRBC 0 k) = BRCC k
        normalize (BRBC 1 k) = BRNE k
        normalize (BRBC 2 k) = BRPL k
        normalize (BRBC 3 k) = BRVC k
        normalize (BRBC 4 k) = BRGE k
        normalize (BRBC 5 k) = BRHC k
        normalize (BRBC 6 k) = BRTC k
        normalize (BRBC 7 k) = BRID k

        normalize (BRBS 0 k) = BRCS k
        normalize (BRBS 1 k) = BREQ k
        normalize (BRBS 2 k) = BRMI k
        normalize (BRBS 3 k) = BRVS k
        normalize (BRBS 4 k) = BRLT k
        normalize (BRBS 5 k) = BRHS k
        normalize (BRBS 6 k) = BRTS k
        normalize (BRBS 7 k) = BRIE k

        normalize (BRSH   k) = BRCC k
        normalize (BRLO   k) = BRCS k

        normalize x          = x

main = do
    quickCheck prop_encodeDecode
