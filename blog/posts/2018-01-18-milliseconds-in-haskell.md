---
title: Milliseconds in haskell
tags: haskell
status: published
---

At the day job we had this "funny" situation where some timestamps are in milliseconds (coming from javascript clients) and some in seconds. Time is notoriously difficult to deal with. In haskell, my goto library is [datetime](http://hackage.haskell.org/package/datetime) which wraps nicely a lot of other primitives.

Dealing with timestamps in seconds is easy: `toSeconds` and `fromSeconds` makes short work.

Dealing with timestamps in *milliseconds* however...
The following snippets requires the packages `datetime` and `time`.


```haskell
import Data.Time.LocalTime as LocalTime
import Data.Time.Clock as Clock
import Data.DateTime as DT

fromMilliseconds :: Int -> UTCTime
fromMilliseconds ts =
    let (s, ms) = ts `divMod` 1000
        utct    = DT.fromSeconds $ fromIntegral s
        day     = Clock.utctDay utct
        (_, _, _, hours, minutes, seconds) = DT.toGregorian utct
        tod     = LocalTime.TimeOfDay
            hours
            minutes
            (fromIntegral seconds + fromIntegral ms * 0.001)
    in  UTCTime day (LocalTime.timeOfDayToTime tod)

toMilliseconds :: UTCTime -> Int
toMilliseconds t =
    let diff = Clock.utctDayTime t
        ms   = truncate (diff * 1000) `rem` 1000
        secs = DT.toSeconds t
    in  fromInteger secs * 1000 + ms
```

I've [created an issue](https://github.com/stackbuilders/datetime/issues/10) on the datetime repository to add these functions, but it seems dead at the moment :/ Hopefully this will make it at some point.
