# Jalaali Haskell

A few Haskell functions for converting Jalaali (Jalali, Persian, Khayyami, Khorshidi, Shamsi) and Gregorian calendar systems to each other.

## About

Jalali calendar is a solar calendar that was used in Persia, variants of which today are still in use in Iran as well as Afghanistan. [Read more on Wikipedia](http://en.wikipedia.org/wiki/Jalali_calendar) or see [Calendar Converter](http://www.fourmilab.ch/documents/calendar/).

Calendar conversion is based on the [algorithm provided by Kazimierz M. Borkowski](http://www.astro.uni.torun.pl/~kb/Papers/EMP/PersianC-EMP.htm) and has a very good performance.

## API

### Types

```haskell
type JalaaliYear = Int
type JalaaliMonth = Int
type JalaaliDay = Int
type GregorianYear = Int
type GregorianMonth = Int
type GregorianDay = Int
type JulianDayNumber = Int
type DayInMarch = Int
type LeapOffset = Int
type JalaaliDate = (JalaaliYear, JalaaliMonth, JalaaliDay)
type GregorianDate = (GregorianYear, GregorianMonth, GregorianDay)
```

### Functions

#### toJalaali

Converts a Gregorian date to Jalaali.

```haskell
toJalaali :: GregorianYear -> GregorianMonth -> GregorianDay -> JalaaliDate
```

#### toGregorian

Converts a Jalaali date to Gregorian.

```haskell
toGregorian :: JalaaliYear -> JalaaliMonth -> JalaaliDay -> GregorianDate
```

#### isValidJalaaliDate

Checkes whether a Jalaali date is valid or not.

```haskell
isValidJalaaliDate :: JalaaliYear -> JalaaliMonth -> JalaaliDay -> Bool
```

#### isJalaaliLeapYear

Checks whether a Jalaali year is leap or not.

```haskell
isJalaaliLeapYear :: JalaaliYear -> Bool
```

#### jalaaliMonthLength

Returns the number of days in a Jalaali year and month.

```haskell
jalaaliMonthLength :: JalaaliYear -> JalaaliMonth -> Int
```

#### jalCal

Determines if a Jalaali year is leap or common, and finds the day in March of the first day of the Jalaali year.

```haskell
jalCal :: JalaaliYear -> (LeapOffset, GregorianYear, DayInMarch)
```

#### j2d

Converts a Jalaali date to Julian Day number.

```haskell
j2d :: JalaaliYear -> JalaaliMonth -> JalaaliDay -> JulianDayNumber
```

#### d2j

Converts a Julian Day number to a Jalaali date.

```haskell
d2j :: JulianDayNumber -> JalaaliDate
```

#### g2d

Converts a Gregorian date to Julian Day number.

```haskell
g2d :: GregorianYear -> GregorianMonth -> GregorianDay -> JulianDayNumber
```

#### d2g

Converts a Julian Day number to a Gregorian date.

```haskell
d2g :: JulianDayNumber -> GregorianDate
```

## License

MIT
