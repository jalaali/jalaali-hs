-- | An Int representing a Jalaali year.
type JalaaliYear = Int
-- | An Int representing a Gregorian year.
type GregorianYear = Int
-- | An Int representing the day in March when converting a Jalaali year to Gregorian.
type DayInMarch = Int
-- | An Int representing number of years since the last leap year.
type LeapOffset = Int

-- Jalaali years starting the 33-year rule.
breaks =  [ -61, 9, 38, 199, 426, 686, 756, 818, 1111, 1181, 1210
          , 1635, 2060, 2097, 2192, 2262, 2324, 2394, 2456, 3178
          ]
firstJump = head (tail breaks) - head breaks -- first jump in breaks

{-|
  This function determines if the Jalaali (Persian) year is
  leap (366-day long) or is the common year (365 days), and
  finds the day in March (Gregorian calendar) of the first
  day of the Jalaali year (jy).

  @param jy Jalaali calendar year (-61 to 3177)
  @return
    leap: number of years since the last leap year (0 to 4)
    gy: Gregorian year of the beginning of Jalaali year
    march: the March day of Farvardin the 1st (1st day of jy)
  @see: http://www.astro.uni.torun.pl/~kb/Papers/EMP/PersianC-EMP.htm
  @see: http://www.fourmilab.ch/documents/calendar/
-}
jalCal :: JalaaliYear -> (LeapOffset, GregorianYear, DayInMarch)
jalCal jy
  | jy <= (-62) = error ("invalid jalaali year " ++ show jy ++ ", should be greater than -62")
  | jy >= 3178 = error ("invalid jalaali year " ++ show jy ++ ", should be less than 3178")
  | otherwise = (leap, gy, dayInMarch)
    where
      gy = jy + 621

      quot4 = (`quot` 4)
      quot33 = (`quot` 33)
      mod33 = (`mod` 33)

      (before, _) = break (jy <) breaks
      n = jy - last before

      jumps = zipWith (-) (drop 1 before) before
      lastJump = last $ firstJump : jumps

      -- Find the limiting years for the Jalaali year jy.
      leapJ' = foldl  (\acc jump -> acc + quot33 jump * 8 + quot4 (mod33 jump)) (-14) jumps
      -- Find the number of leap years from AD 621 to the beginning
      -- of the current Jalaali year in the Persian calendar.
      leapJ'' = leapJ' + quot33 n * 8 + quot4 (mod33 n + 3)
      leapJ = leapJ'' + if mod33 lastJump == 4 && (lastJump - n) == 4 then 1 else 0

      -- And the same in the Gregorian calendar (until the year gy).
      leapG = quot4 gy - quot4 (((gy `quot` 100) + 1) * 3) - 150

      -- Determine the Gregorian date of Farvardin the 1st.
      dayInMarch = 20 + leapJ - leapG

      -- Find how many years have passed since the last leap year.
      n' = n + if lastJump - n < 6 then (-lastJump) + quot33 (lastJump + 4) * 33 else 0
      leap' = (mod33 (n' + 1) - 1) `mod` 4
      leap = if leap' == -1 then 4 else leap'
