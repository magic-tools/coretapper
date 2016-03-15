Coretapper
===

_In memoriam cockatrice.de, mtgimage.com, and many other projects killed by WotC_

DISCLAIMER
---

Coretapper is merely a study in PureScript, please don't deploy it anywhere.

If you deploy it somewhere, please don't tell us about your deployment, so
that when WotC goes turns on community favourite
C&D-[blackmail](http://magiccards.info/scans/en/on/127.jpg)-shit-fan,
all of us can honestly tell them that we shut our deployment down and don't
know of any other deployments of this code.

…

Not like we're going to deploy this code anywhere anyway. Remember, it's
just a study in PureScript.

ABOUT THIS REPOSITORY
---

This repository defines [a format](https://github.com/magic-tools/coretapper/blob/master/docs/MTG/Cards.md#card)
of representing cards and sets, also provide [means](https://github.com/magic-tools/coretapper/blob/master/docs/MTG/Pools.md)
to generate card pools. All that you need to start writing the logic of a certain Magic tool, without worrying about
the data representation and fetching it. Together with running `getJson.sh` from `farseek`, it should get you
up and running on your way to writing a groundbreakingly cool tool for Magic the Gathering world in minutes.

[Example of using it](https://github.com/magic-tools/coretapper/blob/master/src/Main.purs).

Tests? Hm. Nope. Also, all the algorithms are `O(so_slow)`. Pull requests are welcome, especially on the `Data/` side of things.
If we implement functions exported by modules there properly, odds are, stuff is going to get merged into PureScript libs.

LICENSE: WTFPL
---

```
        DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
                    Version 2, December 2004 

 Copyright (C) 2004 Sam Hocevar <sam@hocevar.net> 

 Everyone is permitted to copy and distribute verbatim or modified 
 copies of this license document, and changing it is allowed as long 
 as the name is changed. 

            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION 

  0. You just DO WHAT THE FUCK YOU WANT TO.
```
