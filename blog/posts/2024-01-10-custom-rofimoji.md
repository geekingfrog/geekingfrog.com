---
title: custom rofimoji
status: published
tags:
- tool
---


# Rofimoji?

[rofimoji](https://github.com/fdw/rofimoji) is an emoji picker using rofi so you
can input various emoji using fuzzy search, it looks like:
![rofimoji](https://raw.githubusercontent.com/fdw/rofimoji/main/screenshot.png)

And I recently discovered that one can extend existing data files. So naturally,
I added the most important emojis: the table flippers:

```
(╯°□°）╯︵ ┻━┻ flipping tables
(ノಠ益ಠ)ノ彡┻━┻ raging table flip
┻━┻︵ \(°□°)/ ︵ ┻━┻ flip all tables!
┬─┬ノ( º _ ºノ) put the table back
ʕノ•ᴥ•ʔノ ︵ ┻━┻ bear flipping table
┻━┻︵ヽ(`Д´)ﾉ︵ ┻━┻ flip tables!
(┛◉Д◉) ┛彡┻━┻ fuck this table
(ノಥ,_｣ಥ)ノ彡┻━┻ fuck this table #2
ノ┬─┬ノ ︵ ( \o°o)\ soviet russia table flips you
(┛❍ᴥ❍﻿)┛彡┻━┻ jake flips table
(╯°Д°）╯︵ /(.□ . \) flipping dude over
(/¯◡ ‿ ◡)/¯ ~ ┻━┻ magical table flipping
┬──┬ ¯\_(ツ) fix table
┬─┬﻿ ノ( ゜-゜ノ) patience young grasshopper
(╯°□°)╯︵ ┻━┻ ︵ ╯(°□° ╯) table flipping battle
(ﾉಥДಥ)ﾉ︵┻━┻･/ magic the gathering freakout
┬─┬﻿ ︵ /(.□. \） flipped by table
(/ .□.)\ ︵╰(゜Д゜)╯︵ /(.□. \) hercules flipping tables
(ノ^_^)ノ┻━┻ ┬─┬ ノ( ^_^ノ) I put back tables
┻━┻ ︵ ლ(⌒-⌒ლ) happy table flipping
(ノಠ益ಠ)ノ彡ʞooqǝɔɐɟ rage facebook flip
(╯°□°)╯︵ ʞooqǝɔɐɟ flipping facebook
(ノ ゜Д゜)ノ ︵ ┻━┻ pudgy table flipping
(._.) ~ ︵ ┻━┻ jedi flipping table
```

Place this under `$XDG_DATA_HOME/rofimoji/data/emojis_smileys_emotion.additional.csv` (the filename is important)
and rofimoji should automatically pick these up as additional emojis.

The one gotcha is that rofimoji will consider anything from the start of the line
to the first space as the emoji, and everything until end of line as description
so for multi characters "emojis" like the table flippers, I replaced the spaces
by non breakable spaces and everything works just fine then:

![example of custom table flips](/static/2024-01-04-custom-rofimoji/rofimoji.png)

et voilà, truly a required tool.
