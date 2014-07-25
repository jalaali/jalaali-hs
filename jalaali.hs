-- | An Int representing a Jalaali year.
type JalaaliYear = Int
-- | An Int representing a Gregorian year.
type GregorianYear = Int
-- | An Int representing the day in March when converting a Jalaali year to Gregorian.
type DayInMarch = Int
-- | An Int representing number of years since the last leap year.
type LeapOffset = Int


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
  | jy < (-61) = error ("invalid jalaali year " ++ show jy ++ ", should be greater than -61")
  | jy > 3177 = error ("invalid jalaali year " ++ show jy ++ ", should be less than 3177")
  | otherwise =
      let breaks =  [ -61, 9, 38, 199, 426, 686, 756, 818, 1111, 1181, 1210
                    , 1635, 2060, 2097, 2192, 2262, 2324, 2394, 2456, 3178
                    ] -- Jalaali years starting the 33-year rule.
          gy = jy + 621

          (before, _) = break (jy<) breaks
          n = jy - last before

          jumps = zipWith (-) (drop 1 before) before
          lastJump = last jumps

          -- Find the limiting years for the Jalaali year jy.
          leapJ' = foldl  (\acc jump -> acc +
                                        ((jump `quot` 33) * 8) +
                                        ((jump `mod` 33) `quot` 4)
                          ) (-14) jumps
          -- Find the number of leap years from AD 621 to the beginning
          -- of the current Jalaali year in the Persian calendar.
          leapJ'' = leapJ' + ((n `quot` 33) * 8) + (((n `mod` 33) + 3) `quot` 4)
          leapJ = leapJ'' + if ((lastJump `mod` 33) == 4) && ((lastJump - n) == 4) then 1 else 0

          -- And the same in the Gregorian calendar (until the year gy).
          leapG = (gy `quot` 4) - ((((gy `quot` 100) + 1) * 3) `quot` 4) - 150

          -- Determine the Gregorian date of Farvardin the 1st.
          dayInMarch = 20 + leapJ - leapG

          -- Find how many years have passed since the last leap year.
          n' = n + if (lastJump - n < 6) then (-lastJump) + (((lastJump + 4) `quot` 33) * 33) else 0
          leap' = (((n' + 1) `mod` 33) - 1) `mod` 4
          leap = if leap' == -1 then 4 else leap'

      in (leap, gy, dayInMarch)
