module Data.Time.Calendar.JalaaliSpec (spec) where


import Data.Time.Calendar (CalendarDiffDays(CalendarDiffDays), fromGregorian)
import Data.Time.Calendar.Jalaali
import Test.Hspec


spec :: Spec
spec = do

  describe "toJalaali" $ do
    it "1980-01-01 should be 1358/10/11" $ toJalaali (fromGregorian 1980 1 1) `shouldBe` (1358, 10, 11)
    it "1980-10-10 should be 1359/07/18" $ toJalaali (fromGregorian 1980 10 10) `shouldBe` (1359, 7, 18)
    it "2000-02-02 should be 1378/11/13" $ toJalaali (fromGregorian 2000 2 2) `shouldBe` (1378, 11, 13)
    it "2000-12-12 should be 1379/09/22" $ toJalaali (fromGregorian 2000 12 12) `shouldBe` (1379, 9, 22)
    it "2020-03-30 should be 1399/01/11" $ toJalaali (fromGregorian 2020 3 30) `shouldBe` (1399, 1, 11)
    it "2020-12-30 should be 1399/10/10" $ toJalaali (fromGregorian 2020 12 30) `shouldBe` (1399, 10, 10)
    it "2040-04-30 should be 1419/02/11" $ toJalaali (fromGregorian 2040 4 30) `shouldBe` (1419, 2, 11)
    it "2040-11-30 should be 1419/09/10" $ toJalaali (fromGregorian 2040 11 30) `shouldBe` (1419, 09, 10)

  describe "fromJalaali" $ do
    it "1360/01/01 should be 1981-03-21" $ fromJalaali 1360 1 1 `shouldBe` fromGregorian 1981 3 21
    it "1360/10/10 should be 1981-12-31" $ fromJalaali 1360 10 10 `shouldBe` fromGregorian 1981 12 31
    it "1380/02/02 should be 2001-04-22" $ fromJalaali 1380 2 2 `shouldBe` fromGregorian 2001 4 22
    it "1380/12/12 should be 2002-03-03" $ fromJalaali 1380 12 12 `shouldBe` fromGregorian 2002 03 03
    it "1400/03/30 should be 2021-06-20" $ fromJalaali 1400 3 30 `shouldBe` fromGregorian 2021 6 20
    it "1400/12/29 should be 2022-03-20" $ fromJalaali 1400 12 29 `shouldBe` fromGregorian 2022 03 20
    it "1420/04/30 should be 2041-07-20" $ fromJalaali 1420 4 30 `shouldBe` fromGregorian 2041 7 20
    it "1420/11/30 should be 2042-02-18" $ fromJalaali 1420 11 30 `shouldBe` fromGregorian 2042 02 18
    it "1400/13/32 should be 2022-03-20" $ fromJalaali 1400 13 32 `shouldBe` fromGregorian 2022 03 20
    it "1400/00/00 should be 2021-03-21" $ fromJalaali 1400 0 0 `shouldBe` fromGregorian 2021 03 21

  describe "fromJalaaliValid" $ do
    it "1360/01/01 should be valid" $ fromJalaaliValid 1360 1 1 `shouldBe` (Just $ fromGregorian 1981 3 21)
    it "1360/01/31 should be valid" $ fromJalaaliValid 1360 1 31 `shouldBe` (Just $ fromGregorian 1981 4 20)
    it "1360/01/32 should be invalid" $ fromJalaaliValid 1360 1 32 `shouldBe` Nothing
    it "1380/07/30 should be valid" $ fromJalaaliValid 1380 7 30 `shouldBe` (Just $ fromGregorian 2001 10 22)
    it "1380/07/31 should be invalid" $ fromJalaaliValid 1380 7 31 `shouldBe` Nothing
    it "1400/11/30 should be valid" $ fromJalaaliValid 1400 11 30 `shouldBe` (Just $ fromGregorian 2022 2 19)
    it "1400/11/31 should be invalid" $ fromJalaaliValid 1400 11 31 `shouldBe` Nothing
    it "1420/12/30 should be valid" $ fromJalaaliValid 1420 12 30 `shouldBe` (Just $ fromGregorian 2042 3 20)
    it "1420/12/31 should be invalid" $ fromJalaaliValid 1420 12 31 `shouldBe` Nothing

  describe "showJalaali" $ do
    it "should format with zero pad" $ showJalaali (fromJalaali 1400 1 1) `shouldBe` "1400/01/01"
    it "should format using slash character" $ showJalaali (fromJalaali 1400 12 29) `shouldBe` "1400/12/29"

  describe "jalaaliMonthLength" $ do
    it "1360/01 should be 31" $ jalaaliMonthLength 1360 1 `shouldBe` 31
    it "1360/06 should be 31" $ jalaaliMonthLength 1360 6 `shouldBe` 31
    it "1380/07 should be 30" $ jalaaliMonthLength 1380 7 `shouldBe` 30
    it "1380/12 should be 29" $ jalaaliMonthLength 1380 12 `shouldBe` 29
    it "1400/03 should be 31" $ jalaaliMonthLength 1400 3 `shouldBe` 31
    it "1400/09 should be 30" $ jalaaliMonthLength 1400 9 `shouldBe` 30
    it "1420/10 should be 30" $ jalaaliMonthLength 1420 10 `shouldBe` 30
    it "1400/12 should be 30" $ jalaaliMonthLength 1420 12 `shouldBe` 30

  describe "addJalaaliMonthsClip" $ do
    it "1400/05/01 + 1m should be 1400/06/01" $ addJalaaliMonthsClip 1 (fromJalaali 1400 5 1) `shouldBe` (fromJalaali 1400 6 1)
    it "1400/05/31 + 1m should be 1400/06/31" $ addJalaaliMonthsClip 1 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1400 6 31)
    it "1400/05/31 + 2m should be 1400/07/30" $ addJalaaliMonthsClip 2 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1400 7 30)
    it "1400/05/31 + 3m should be 1400/08/30" $ addJalaaliMonthsClip 3 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1400 8 30)
    it "1400/05/31 + 6m should be 1400/11/30" $ addJalaaliMonthsClip 6 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1400 11 30)
    it "1400/05/31 + 7m should be 1400/12/29" $ addJalaaliMonthsClip 7 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1400 12 29)
    it "1400/05/31 + 8m should be 1401/01/31" $ addJalaaliMonthsClip 8 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1401 1 31)
    it "1400/05/31 + 12m should be 1401/05/31" $ addJalaaliMonthsClip 12 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1401 5 31)
    it "1400/05/31 + 19m should be 1401/12/29" $ addJalaaliMonthsClip 19 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1401 12 29)
    it "1400/05/31 + 31m should be 1402/12/29" $ addJalaaliMonthsClip 31 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1402 12 29)
    it "1400/05/31 + 43m should be 1403/12/30" $ addJalaaliMonthsClip 43 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1403 12 30)
    it "1400/05/01 + 0m should be 1400/05/01" $ addJalaaliMonthsClip 0 (fromJalaali 1400 5 1) `shouldBe` (fromJalaali 1400 5 1)
    it "1400/05/01 - 1m should be 1400/04/01" $ addJalaaliMonthsClip (-1) (fromJalaali 1400 5 1) `shouldBe` (fromJalaali 1400 4 1)
    it "1400/05/31 - 1m should be 1400/04/31" $ addJalaaliMonthsClip (-1) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1400 4 31)
    it "1400/05/31 - 4m should be 1400/01/31" $ addJalaaliMonthsClip (-4) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1400 1 31)
    it "1400/05/31 - 5m should be 1399/12/30" $ addJalaaliMonthsClip (-5) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1399 12 30)
    it "1400/05/31 - 6m should be 1400/11/30" $ addJalaaliMonthsClip (-6) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1399 11 30)
    it "1400/05/31 - 12m should be 1399/05/31" $ addJalaaliMonthsClip (-12) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1399 5 31)
    it "1400/05/31 - 17m should be 1398/12/29" $ addJalaaliMonthsClip (-17) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1398 12 29)
    it "1400/05/31 - 29m should be 1397/12/29" $ addJalaaliMonthsClip (-29) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1397 12 29)
    it "1400/05/31 - 41m should be 1396/12/29" $ addJalaaliMonthsClip (-41) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1396 12 29)
    it "1400/05/31 - 53m should be 1395/12/30" $ addJalaaliMonthsClip (-53) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1395 12 30)

  describe "addJalaaliMonthsRollOver" $ do
    it "1400/05/01 + 1m should be 1400/06/01" $ addJalaaliMonthsRollOver 1 (fromJalaali 1400 5 1) `shouldBe` (fromJalaali 1400 6 1)
    it "1400/05/31 + 1m should be 1400/06/31" $ addJalaaliMonthsRollOver 1 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1400 6 31)
    it "1400/05/31 + 2m should be 1400/08/01" $ addJalaaliMonthsRollOver 2 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1400 8 1)
    it "1400/05/31 + 3m should be 1400/09/01" $ addJalaaliMonthsRollOver 3 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1400 9 1)
    it "1400/05/31 + 6m should be 1400/12/01" $ addJalaaliMonthsRollOver 6 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1400 12 1)
    it "1400/05/31 + 7m should be 1401/01/02" $ addJalaaliMonthsRollOver 7 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1401 1 2)
    it "1400/05/31 + 8m should be 1401/01/31" $ addJalaaliMonthsRollOver 8 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1401 1 31)
    it "1400/05/31 + 12m should be 1401/05/31" $ addJalaaliMonthsRollOver 12 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1401 5 31)
    it "1400/05/31 + 19m should be 1402/01/02" $ addJalaaliMonthsRollOver 19 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1402 1 2)
    it "1400/05/31 + 31m should be 1403/01/02" $ addJalaaliMonthsRollOver 31 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1403 1 2)
    it "1400/05/31 + 43m should be 1404/01/1" $ addJalaaliMonthsRollOver 43 (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1404 1 1)
    it "1400/05/01 + 0m should be 1400/05/01" $ addJalaaliMonthsRollOver 0 (fromJalaali 1400 5 1) `shouldBe` (fromJalaali 1400 5 1)
    it "1400/05/01 - 1m should be 1400/04/01" $ addJalaaliMonthsRollOver (-1) (fromJalaali 1400 5 1) `shouldBe` (fromJalaali 1400 4 1)
    it "1400/05/31 - 1m should be 1400/04/31" $ addJalaaliMonthsRollOver (-1) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1400 4 31)
    it "1400/05/31 - 4m should be 1400/01/31" $ addJalaaliMonthsRollOver (-4) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1400 1 31)
    it "1400/05/31 - 5m should be 1400/01/01" $ addJalaaliMonthsRollOver (-5) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1400 1 1)
    it "1400/05/31 - 6m should be 1400/12/01" $ addJalaaliMonthsRollOver (-6) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1399 12 1)
    it "1400/05/31 - 12m should be 1399/05/31" $ addJalaaliMonthsRollOver (-12) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1399 5 31)
    it "1400/05/31 - 17m should be 1399/01/02" $ addJalaaliMonthsRollOver (-17) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1399 1 2)
    it "1400/05/31 - 29m should be 1398/01/02" $ addJalaaliMonthsRollOver (-29) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1398 1 2)
    it "1400/05/31 - 41m should be 1397/01/02" $ addJalaaliMonthsRollOver (-41) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1397 1 2)
    it "1400/05/31 - 53m should be 1396/01/01" $ addJalaaliMonthsRollOver (-53) (fromJalaali 1400 5 31) `shouldBe` (fromJalaali 1396 1 1)

  describe "addJalaaliYearsClip" $ do
    it "1399/12/01 + 1y should be 1400/12/01" $ addJalaaliYearsClip 1 (fromJalaali 1399 12 1) `shouldBe` (fromJalaali 1400 12 1)
    it "1399/12/30 + 1y should be 1400/12/29" $ addJalaaliYearsClip 1 (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1400 12 29)
    it "1399/12/30 + 2y should be 1401/12/29" $ addJalaaliYearsClip 2 (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1401 12 29)
    it "1399/12/30 + 3y should be 1402/12/29" $ addJalaaliYearsClip 3 (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1402 12 29)
    it "1399/12/30 + 4y should be 1403/12/30" $ addJalaaliYearsClip 4 (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1403 12 30)
    it "1399/12/30 + 5y should be 1404/12/29" $ addJalaaliYearsClip 5 (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1404 12 29)
    it "1399/12/30 + 0y should be 1399/12/30" $ addJalaaliYearsClip 0 (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1399 12 30)
    it "1399/12/30 - 1y should be 1398/12/29" $ addJalaaliYearsClip (-1) (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1398 12 29)
    it "1399/12/30 - 2y should be 1397/12/29" $ addJalaaliYearsClip (-2) (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1397 12 29)
    it "1399/12/30 - 3y should be 1396/12/29" $ addJalaaliYearsClip (-3) (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1396 12 29)
    it "1399/12/30 - 4y should be 1395/12/30" $ addJalaaliYearsClip (-4) (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1395 12 30)
    it "1399/12/30 - 5y should be 1394/12/29" $ addJalaaliYearsClip (-5) (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1394 12 29)

  describe "addJalaaliYearsRollOver" $ do
    it "1399/12/01 + 1y should be 1400/12/01" $ addJalaaliYearsRollOver 1 (fromJalaali 1399 12 1) `shouldBe` (fromJalaali 1400 12 1)
    it "1399/12/30 + 1y should be 1401/01/01" $ addJalaaliYearsRollOver 1 (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1401 1 1)
    it "1399/12/30 + 2y should be 1402/01/01" $ addJalaaliYearsRollOver 2 (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1402 1 1)
    it "1399/12/30 + 3y should be 1403/01/01" $ addJalaaliYearsRollOver 3 (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1403 1 1)
    it "1399/12/30 + 4y should be 1403/12/30" $ addJalaaliYearsRollOver 4 (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1403 12 30)
    it "1399/12/30 + 5y should be 1405/01/01" $ addJalaaliYearsRollOver 5 (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1405 1 1)
    it "1399/12/30 + 0y should be 1399/12/30" $ addJalaaliYearsRollOver 0 (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1399 12 30)
    it "1399/12/30 - 1y should be 1399/01/01" $ addJalaaliYearsRollOver (-1) (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1399 1 1)
    it "1399/12/30 - 2y should be 1398/01/01" $ addJalaaliYearsRollOver (-2) (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1398 1 1)
    it "1399/12/30 - 3y should be 1397/01/01" $ addJalaaliYearsRollOver (-3) (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1397 1 1)
    it "1399/12/30 - 4y should be 1395/12/30" $ addJalaaliYearsRollOver (-4) (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1395 12 30)
    it "1399/12/30 - 5y should be 1395/01/01" $ addJalaaliYearsRollOver (-5) (fromJalaali 1399 12 30) `shouldBe` (fromJalaali 1395 1 1)

  describe "addJalaaliDurationClip" $ do
    it "1399/05/31 + 7m1d should be 1400/01/01" $ addJalaaliDurationClip (CalendarDiffDays 7 1) (fromJalaali 1399 5 31) `shouldBe` (fromJalaali 1400 1 1)
    it "1399/05/31 + -5m1d should be 1399/01/01" $ addJalaaliDurationClip (CalendarDiffDays (-5) 1) (fromJalaali 1399 5 31) `shouldBe` (fromJalaali 1399 1 1)

  describe "addJalaaliDurationRollOver" $ do
    it "1399/05/31 + 7m1d should be 1400/01/02" $ addJalaaliDurationRollOver (CalendarDiffDays 7 1) (fromJalaali 1399 5 31) `shouldBe` (fromJalaali 1400 1 2)
    it "1399/05/31 + -5m1d should be 1399/01/02" $ addJalaaliDurationRollOver (CalendarDiffDays (-5) 1) (fromJalaali 1399 5 31) `shouldBe` (fromJalaali 1399 1 3)

  describe "isJalaaliLeapYear" $ do
    it "1393 should be common" $ isJalaaliLeapYear 1393 `shouldBe` False
    it "1394 should be common" $ isJalaaliLeapYear 1394 `shouldBe` False
    it "1395 should be leap" $ isJalaaliLeapYear 1395 `shouldBe` True
    it "1396 should be common" $ isJalaaliLeapYear 1396 `shouldBe` False
    it "1397 should be common" $ isJalaaliLeapYear 1397 `shouldBe` False
    it "1398 should be common" $ isJalaaliLeapYear 1398 `shouldBe` False
    it "1399 should be leap" $ isJalaaliLeapYear 1399 `shouldBe` True
    it "1400 should be common" $ isJalaaliLeapYear 1400 `shouldBe` False
    it "1401 should be common" $ isJalaaliLeapYear 1401 `shouldBe` False
    it "1402 should be common" $ isJalaaliLeapYear 1402 `shouldBe` False
    it "1403 should be leap" $ isJalaaliLeapYear 1403 `shouldBe` True
    it "1404 should be common" $ isJalaaliLeapYear 1404 `shouldBe` False
