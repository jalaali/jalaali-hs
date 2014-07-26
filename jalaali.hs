-- | An Int representing a Jalaali year.
type JalaaliYear = Int

-- | An Int representing a Jalaali month (1-based).
type JalaaliMonth = Int

-- | An Int representing a Jalaali day.
type JalaaliDay = Int

-- | An Int representing a Gregorian year.
type GregorianYear = Int

-- | An Int representing a Gregorian month (1-based).
type GregorianMonth = Int

-- | An Int representing a Gregorian day.
type GregorianDay = Int

-- | An Int representing a Julian Day Number
type JulianDayNumber = Int

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
  | jy < (-61) = error ("invalid jalaali year " ++ show jy ++ ", should be >= -61")
  | jy > 3177 = error ("invalid jalaali year " ++ show jy ++ ", should be <= 3177")
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

{-|
  Converts a date of the Jalaali calendar to the Julian Day number.

  @param jy Jalaali year (1 to 3100)
  @param jm Jalaali month (1 to 12)
  @param jd Jalaali day (1 to 29/31)
  @return Julian Day number
-}
j2d :: (JalaaliYear, JalaaliMonth, JalaaliDay) -> JulianDayNumber
j2d (jy, jm, jd) = jdn + (jm - 1) * 31 - (jm `quot` 7) * (jm - 7) + jd - 1
  where
    (leap, gy, dayInMarch) = jalCal jy
    jdn = g2d (gy, 3, dayInMarch)

{-|
  Converts the Julian Day number to a date in the Jalaali calendar.

  @param jdn Julian Day number
  @return
    jy: Jalaali year (1 to 3100)
    jm: Jalaali month (1 to 12)
    jd: Jalaali day (1 to 29/31)
-}
d2j :: JulianDayNumber -> (JalaaliYear, JalaaliMonth, JalaaliDay)
d2j jdn = (jy, jm, jd)
  where
    (gy, _, _) = d2g jdn
    jy' = gy - 621
    (leap, _, dayInMarch) = jalCal jy'
    jdn1f = g2d (gy, 3, dayInMarch)
    k' = jdn - jdn1f
    k = if k' >= 0 && k' <= 185
        then k'
        else if k' >= 0
          then k' - 186
          else k' + 179 + if leap == 1 then 1 else 0
    jy = jy' - if k' < 0 then 1 else 0 -- Previous Jalaali year.
    jm =  if k' >= 0 && k' <= 185
          then 1 + (k `quot` 31)
          else 7 + (k `quot` 30)
    jd =  if k' >= 0 && k' <= 185
          then 1 + (k `mod` 31)
          else 1 + (k `mod` 30)


{-|
  Calculates the Julian Day number from Gregorian or Julian
  calendar dates. This integer number corresponds to the noon of
  the date (i.e. 12 hours of Universal Time).
  The procedure was tested to be good since 1 March, -100100 (of both
  calendars) up to a few million years into the future.

  @param gy Calendar year (years BC numbered 0, -1, -2, ...)
  @param gm Calendar month (1 to 12)
  @param gd Calendar day of the month (1 to 28/29/30/31)
  @return Julian Day number
-}
g2d :: (GregorianYear, GregorianMonth, GregorianDay) -> JulianDayNumber
g2d (gy, gm, gd) =
  d - ((((gy + 100100 + ((gm - 8) `quot` 6)) `quot` 100) * 3) `quot` 4) + 752
  where
    d = ((gy + ((gm - 8) `quot` 6) + 100100) * 1461) `quot` 4 +
        (153 * ((gm + 9) `mod` 12) + 2) `quot` 5 +
        gd - 34840408

{-|
  Calculates Gregorian and Julian calendar dates from the Julian Day number
  (jdn) for the period since jdn=-34839655 (i.e. the year -100100 of both
  calendars) to some millions years ahead of the present.

  @param jdn Julian Day number
  @return
    gy: Calendar year (years BC numbered 0, -1, -2, ...)
    gm: Calendar month (1 to 12)
    gd: Calendar day of the month M (1 to 28/29/30/31)
-}
d2g :: JulianDayNumber -> (GregorianYear, GregorianMonth, GregorianDay)
d2g jdn = (gy, gm, gd)
  where
    j' = 4 * jdn + 139361631
    j = j' + ((((4 * jdn + 183187720) `quot` 146097) * 3) `quot` 4) * 4 - 3908
    i = ((j `mod` 1461) `quot` 4) * 5 + 308
    gd = ((i `mod` 153) `quot` 5) + 1
    gm = ((i `quot` 153) `mod` 12) + 1
    gy = (j `quot` 1461) - 100100 + ((8 - gm) `quot` 6)
