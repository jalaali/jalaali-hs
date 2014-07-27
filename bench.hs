import Data.Time.Calendar.Jalaali
import Data.Time.Clock
import Control.DeepSeq

main = do
  let n = 1000000

  let jd = cycle [toGregorian y m d | y <- [1..3000], m <- [1..12], d <- [1..30]]
  let gd = cycle [toJalaali y m d | y <- [560..3560], m <- [1..12], d <- [1..30]]
  let jl = cycle [isJalaaliLeapYear y | y <- [1..3000]]
  let jv = cycle [isValidJalaaliDate y m d | y <- [1..3000], m <- [1..13], d <- [1..32]]

  start1 <- getCurrentTime
  end1 <- take n jd `deepseq` getCurrentTime
  putStrLn $ show n ++ " calls of toGregorian took " ++ show (diffUTCTime end1 start1)

  start2 <- getCurrentTime
  end2 <- take n gd `deepseq` getCurrentTime
  putStrLn $ show n ++ " calls of toJalaali took " ++ show (diffUTCTime end2 start2)

  start3 <- getCurrentTime
  end3 <- take n jl `deepseq` getCurrentTime
  putStrLn $ show n ++ " calls of isJalaaliLeapYear took " ++ show (diffUTCTime end3 start3)

  start4 <- getCurrentTime
  end4 <- take n jv `deepseq` getCurrentTime
  putStrLn $ show n ++ " calls of isJalaaliLeapYear took " ++ show (diffUTCTime end4 start4)
