import Test.QuickCheck

import Instructions

prop_encodeDecode :: Instruction -> Bool
prop_encodeDecode x = normalize x' == normalize x
    where
        (x', []) = decode (encode x)

        normalize (BRSH k) = BRBC 0 k
        normalize (BRCC k) = BRBC 0 k
        normalize (BRNE k) = BRBC 1 k
        normalize (BRPL k) = BRBC 2 k
        normalize (BRVC k) = BRBC 3 k
        normalize (BRGE k) = BRBC 4 k
        normalize (BRHC k) = BRBC 5 k
        normalize (BRTC k) = BRBC 6 k
        normalize (BRID k) = BRBC 7 k

        normalize (BRLO k) = BRBS 0 k
        normalize (BRCS k) = BRBS 0 k
        normalize (BREQ k) = BRBS 1 k
        normalize (BRMI k) = BRBS 2 k
        normalize (BRVS k) = BRBS 3 k
        normalize (BRLT k) = BRBS 4 k
        normalize (BRHS k) = BRBS 5 k
        normalize (BRTS k) = BRBS 6 k
        normalize (BRIE k) = BRBS 7 k

        normalize CLC      = BCLR 0
        normalize CLZ      = BCLR 1
        normalize CLN      = BCLR 2
        normalize CLV      = BCLR 3
        normalize CLS      = BCLR 4
        normalize CLH      = BCLR 5
        normalize CLT      = BCLR 6
        normalize CLI      = BCLR 7

        normalize x        = x

main = do
    quickCheck prop_encodeDecode
