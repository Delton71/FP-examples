-- -----/ Pythagorean Triplets /-----

pythagoreanTriplets :: [(Integer, Integer, Integer)]
pythagoreanTriplets = [(x,y,z) | z <- [1..], y <- [1..z], x <- [1..y], 
                                x^2 + y^2 == z^2]


-- example
x = take 10 pythagoreanTriplets
-- x = [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),
--      (12,16,20),(15,20,25),(7,24,25),(10,24,26),(20,21,29)]
