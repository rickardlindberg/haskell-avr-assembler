import Test.QuickCheck

import Instructions

prop_encodeDecode :: Instruction -> Bool
prop_encodeDecode randomInstruction =
    let (decodedInstruction, []) = decode (encode randomInstruction)
    in  decodedInstruction == randomInstruction

main = do
    quickCheck prop_encodeDecode
