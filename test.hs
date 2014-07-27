import Data.Time.Calendar.Jalaali

main = do

  toJalaali 1981 8 17 `eq` (1360, 5, 26)
  toGregorian 1360 5 26 `eq` (1981, 8, 17)

  toJalaali 2013 1 10 `eq` (1391, 10, 21)
  toGregorian 1391 10 21 `eq` (2013, 1, 10)

  toGregorian 1393 5 4 `eq` (2014, 7, 26)
  toJalaali 2014 7 26 `eq` (1393, 5, 4)

  isValidJalaaliDate (-62) 12 29 `eq` False
  isValidJalaaliDate (-61) 1 1 `eq` True
  isValidJalaaliDate 3178 1 1 `eq` False
  isValidJalaaliDate 3177 12 29 `eq` True
  isValidJalaaliDate 1393 0 1 `eq` False
  isValidJalaaliDate 1393 13 1 `eq` False
  isValidJalaaliDate 1393 1 0 `eq` False
  isValidJalaaliDate 1393 1 32 `eq` False
  isValidJalaaliDate 1393 1 31 `eq` True
  isValidJalaaliDate 1393 11 31 `eq` False
  isValidJalaaliDate 1393 11 30 `eq` True
  isValidJalaaliDate 1393 12 30 `eq` False
  isValidJalaaliDate 1393 12 29 `eq` True
  isValidJalaaliDate 1395 12 30 `eq` True

  isJalaaliLeapYear 1393 `eq` False
  isJalaaliLeapYear 1394 `eq` False
  isJalaaliLeapYear 1395 `eq` True
  isJalaaliLeapYear 1396 `eq` False

  jalaaliMonthLength 1393 1 `eq` 31
  jalaaliMonthLength 1393 4 `eq` 31
  jalaaliMonthLength 1393 6 `eq` 31
  jalaaliMonthLength 1393 7 `eq` 30
  jalaaliMonthLength 1393 10 `eq` 30
  jalaaliMonthLength 1393 12 `eq` 29
  jalaaliMonthLength 1394 12 `eq` 29
  jalaaliMonthLength 1395 12 `eq` 30

eq :: (Eq a, Show a) => a -> a -> IO ()
a `eq` e
  | a == e = return ()
  | otherwise = error $ "Assert FAILED\nactual: " ++ show a ++ "\nexpected: " ++ show e
