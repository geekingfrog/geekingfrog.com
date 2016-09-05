---
title: How many pings were dropped?
tags: haskell, parsing
status: published
---

## How bad my wifi is ?
I moved to London, and in the process, experienced some pretty weird wifi signals. The connection would almost drop for a few minutes and then back to normal. And this cycle would repeat itself every 5~10 minutes. Pretty annoying. (turn out I could fix that with a kernel parameter)

So I used to start `ping google.com | tee -a ping.dat` and I used this as a excuse to try some haskell.

## Parsing with Parsec
A sample of the file look like:

```
PING google.com (74.125.195.102) 56(84) bytes of data.
64 bytes from wj-in-f102.1e100.net (74.125.195.102): icmp_seq=1 ttl=50 time=39.4 ms
64 bytes from wj-in-f102.1e100.net (74.125.195.102): icmp_seq=2 ttl=50 time=23.0 ms
[...]
64 bytes from lhr14s24-in-f14.1e100.net (216.58.210.78): icmp_seq=351 ttl=58 time=196 ms
64 bytes from 216.58.210.78: icmp_seq=352 ttl=58 time=282 ms
64 bytes from lhr14s24-in-f14.1e100.net (216.58.210.78): icmp_seq=358 ttl=58 time=474 ms
```

So the first think I had to was to learn how to parse it. I found the very good chapter of [real world haskell](http://book.realworldhaskell.org/) about [parsec](http://book.realworldhaskell.org/read/using-parsec.html). The documentation on hoogle is only good if one already knows how to use the library, so this chapter was very useful.

## Regular expressions?
I could've done this with regular expression. Using a library like parsec for this probably overkill but it was a learning exercise anyway.
Another reason is readability and maintainablity. Compare the following javasrcipt snippet:
```
var r = /icmp_seq=(\d+) ttl=\d+ time=(\d+)/;
var matched = line.match(r);
if(matched.length) {
  var seq = parseInt(matched[1]);
  var time = parseInt(matched[2]);
}
```

With the haskell version:
```
line :: GenParser Char st (Int, Float)
line = do
	string "icmp_seq="
    seq <- positiveNatural
    spaces
    many (noneOf " ")
    string "time="
    time <- positiveFloat
    return (seq, time)
```


The haskell version is a bit longer but much more readable too. The helper functions `positiveNatural` and `positiveFloat` are doing the same as the javascript `parseInt` function, given below for completness:

```
positiveNatural :: CharParser () Int
positiveNatural =
  foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

positiveFloat :: CharParser () Float
positiveFloat = do
  s <- getInput
  case (readFloat s) of
    [(n, s')] -> n <$ setInput s'
    _         -> empty
```

The full code can be found on this [github repo](https://github.com/geekingfrog/learnHaskell/tree/master/droppedPing). I also took some time to understand and use the *funky* `Applicative` notation instead of the `do` notation above.

## Conclusion
I learned how to parse files in haskell, something very common and useful. Parsing with Parsec also lead to some pretty readable and maintable code, which is a major reason I'm interested in haskell.

Although, I'm not sure how it works under the hood (mostly, regarding memory usage), I'll keep that for a later exercise.



