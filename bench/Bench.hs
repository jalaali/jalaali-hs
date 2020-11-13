import Data.Time.Calendar
import Data.Time.Calendar.Jalaali
import Data.Time.Clock
import Control.DeepSeq


main :: IO ()
main = do
  start0 <- getCurrentTime

  let n = 1000000

  let fj = [fromJalaali y m d | y <- [1..3000], m <- [1..12], d <- [1..30]]
  let fg = [fromGregorian y m d | y <- [560..3560], m <- [1..12], d <- [1..30]]
  let tj = map toJalaali fg
  let jl = [isJalaaliLeapYear y | y <- [1..3000], _ <- [1..12] :: [Int], _ <- [1..30] :: [Int]]
  let jv = [fromJalaaliValid y m d | y <- [1..3000], m <- [1..13], d <- [1..32]]

  start1 <- getCurrentTime
  end1 <- take n fj `deepseq` getCurrentTime
  putStrLn $ show n ++ " calls of fromJalaali took " ++ show (diffUTCTime end1 start1)

  start2 <- fg `deepseq` getCurrentTime
  end2 <- take n tj `deepseq` getCurrentTime
  putStrLn $ show n ++ " calls of toJalaali took " ++ show (diffUTCTime end2 start2)

  start3 <- getCurrentTime
  end3 <- take n jl `deepseq` getCurrentTime
  putStrLn $ show n ++ " calls of isJalaaliLeapYear took " ++ show (diffUTCTime end3 start3)

  start4 <- getCurrentTime
  end4 <- take n jv `deepseq` getCurrentTime
  putStrLn $ show n ++ " calls of fromJalaaliValid took " ++ show (diffUTCTime end4 start4)

  end0 <- getCurrentTime
  putStrLn $ "total benchmark time: " ++ show (diffUTCTime end0 start0)
