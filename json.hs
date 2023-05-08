data JSONItem = B Int |A String deriving (a)


fizzbuzz :: Int -> [JSONItem]
fizzbuzz x
  | x <= 1 = []
  | (mod x 3 == 0) && (mod x 5 == 0) = [(A "fizzbuzz")] + (fizzbuzz $ x - 1)
  | (mod x 3 == 0) = [(A "fizz")] + (fizzbuzz $ x - 1)
  | (mod x 5 == 0) = [(A "buzz")] + (fizzbuzz $ x - 1)
  | otherwise = [B x] + (fizzbuzz $ x - 1)
