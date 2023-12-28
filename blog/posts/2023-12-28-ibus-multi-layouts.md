---
title: Ibus with multiple layouts
status: published
tags:
- linux
- TIL
---

# Weird layout adventures

Very recently, a [new keyboard layout](https://ergol.org/) caught my eye, it aims
to be efficient to write french, english and also to code with it.
This is pretty much my usecase, so I'm going to learn it. I've got a bit tired
of bépo, which has some issues, especially for coding, and even worse with vim.

# Install

[Kalamine](https://github.com/fabi1cazenave/kalamine/) is a very cool tool to install
new layout. The input is quite interesting, since it's an ascii representation
of the keyboard, so it also serves as a visual aid.

Installing the layout is trivial with it. The one gotcha is that any upgrade to the xkb
system will wipe out this new layout, but that doesn't happen very often.

As an aside, kalamine has a command to spawn a local server so you can try a layout
in a browser locally, which is **very** cool.

# IBUS

I use [ibus](https://wiki.archlinux.org/title/IBus) to manage different layouts, also
because it integrates with libpinyin to type chinese.

Atfer installing ergo-l, I could switch using `setxkbmap fr -variant ergol`, but the menu
from ibus wouldn't list it.
One need to add the following entry to `/usr/share/ibus/component/simple.xml`

```xml
<engine>
    <name>xkb:fr:ergol:fra</name>
    <language>fr</language>
    <license>WTFPL</license>
    <author>Fabien Cazenave</author>
    <layout>fr</layout>
    <layout_variant>ergol</layout_variant>
    <longname>French (Ergo-L)</longname>
    <description>French (Ergo-L)</description>
    <icon>ibus-keyboard</icon>
    <rank>1</rank>
</engine>
```

and after a restart of ibus, voilà !


# Ergo-L
Also, FYI, this is the ergol layout (subject to some minor changes):

```
base: |
  ╭╌╌╌╌╌┰─────┬─────┬─────┬─────┬─────┰─────┬─────┬─────┬─────┬─────┰╌╌╌╌╌┬╌╌╌╌╌╮
  ┆ ~   ┃ ! ¡ │ « ‘ │ » ’ │ $ ¢ │ % ‰ ┃ ^   │ &   │ *   │ (   │ )   ┃ _ – ┆ + ± ┆
  ┆ `   ┃ 1 „ │ 2 “ │ 3 ” │ 4 £ │ 5 € ┃ 6 ¥ │ 7 ¤ │ 8 § │ 9 ¶ │ 0 ° ┃ / ÷ ┆ = ≠ ┆
  ╰╌╌╌╌╌╂─────┼─────┼─────┼─────┼─────╂─────┼─────┼─────┼─────┼─────╂╌╌╌╌╌┼╌╌╌╌╌┤
  ·     ┃ Q   │ C   │ O   │ P   │ W   ┃ J   │ M   │ D   │*¨   │ F   ┃ {   ┆ }   ┆
  ·     ┃   â │   ç │   œ │   ô │     ┃     │   µ │   _ │** ` │   ŭ ┃ [   ┆ ]   ┆
  ·     ┠─────┼─────┼─────┼─────┼─────╂─────┼─────┼─────┼─────┼─────╂╌╌╌╌╌┼╌╌╌╌╌┤
  ·     ┃ A   │ S   │ E   │ N   │ ; • ┃ L   │ R   │ T   │ I   │ U   ┃ "   ┆ |   ┆
  ·     ┃   à │   é │   è │   ê │ , · ┃     │     │   î │   û │   ù ┃ '   ┆ \   ┆
  ╭╌╌╌╌╌╂─────┼─────┼─────┼─────┼─────╂─────┼─────┼─────┼─────┼─────╂╌╌╌╌╌┴╌╌╌╌╌╯
  ┆ >   ┃ Z   │ X   │ ? ¿ │ V   │ B   ┃ :   │ H   │ G   │ Y   │ K   ┃           ·
  ┆ <   ┃   æ │   ß │ - — │   ñ │     ┃ . … │     │     │     │     ┃           ·
  ╰╌╌╌╌╌┸─────┴─────┴─────┴─────┴─────┸─────┴─────┴─────┴─────┴─────┚ · · · · · ·

altgr: |
  ╭╌╌╌╌╌┰─────┬─────┬─────┬─────┬─────┰─────┬─────┬─────┬─────┬─────┰╌╌╌╌╌┬╌╌╌╌╌╮
  ┆     ┃   ¹ │   ² │   ³ │   ⁴ │   ⁵ ┃   ⁶ │   ⁷ │   ⁸ │   ⁹ │   ⁰ ┃     ┆     ┆
  ┆     ┃   ₁ │   ₂ │   ₃ │   ₄ │   ₅ ┃   ₆ │   ₇ │   ₈ │   ₉ │   ₀ ┃     ┆     ┆
  ╰╌╌╌╌╌╂─────┼─────┼─────┼─────┼─────╂─────┼─────┼─────┼─────┼─────╂╌╌╌╌╌┼╌╌╌╌╌┤
  ·     ┃     │   ≤ │   ≥ │  *¤ │   ‰ ┃  *^ │  *µ │   × │  *´ │  *` ┃     ┆     ┆
  ·     ┃   @ │   < │   > │   $ │   % ┃   ^ │   & │   * │   ' │   ` ┃     ┆     ┆
  ·     ┠─────┼─────┼─────┼─────┼─────╂─────┼─────┼─────┼─────┼─────╂╌╌╌╌╌┼╌╌╌╌╌┤
  ·     ┃     │   ⁽ │   ⁾ │     │   ≠ ┃  */ │   ± │   — │   ÷ │  *¨ ┃     ┆     ┆
  ·     ┃   { │   ( │   ) │   } │   = ┃   \ │   + │   - │   / │   " ┃     ┆     ┆
  ╭╌╌╌╌╌╂─────┼─────┼─────┼─────┼─────╂─────┼─────┼─────┼─────┼─────╂╌╌╌╌╌┴╌╌╌╌╌╯
  ┆     ┃  *~ │     │     │   – │     ┃   ¦ │   ¬ │  *¸ │     │     ┃           ·
  ┆     ┃   ~ │   [ │   ] │   _ │   # ┃   | │   ! │   ; │   : │   ? ┃           ·
  ╰╌╌╌╌╌┸─────┴─────┴─────┴─────┴─────┸─────┴─────┴─────┴─────┴─────┚ · · · · · ·
```
