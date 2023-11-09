
-- -----/ a) xor /-----

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

-- -----/ b) maj_3 /-----

-- if count(True) >= 3 // 2 -> return True

maj3 :: Bool -> Bool -> Bool -> Bool
maj3 True True _ = True
maj3 True _ True = True
maj3 _ True True = True
maj3 _ _ _ = False
