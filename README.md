## Advent Of Code
Not necessarily beauty incarnate, the code you'll find in this repository is wild, romantic nature untamed. It is water: sometimes a quiet stream of art, droplets of learning, an ocean of unknown green and blue to be discovered. Other times it will be hideous or hide dark creatures below the sparkling surface.

You aren't supposed to be reading this anyway. The only reason this is not on my private git server is because it is possible that I want to discuss with friends and compare our approaches.

See [https://adventofcode.com](https://adventofcode.com) if you'd like to join in the fun.

Note that I intend to use this to experiment, try new features or hacky things, or learn new languages. So most of the code will not be representative of my quality production code.

## The Journey Of The Cursed
'twas the end of the decade, yet life was to continue just as it always did. When a cursed artifact is nearby, you can feel a tingle but only when you touch it, you completely understand just how cursed it actually is. That's why I embarked on the journey through several languages.  
And the fun thing about cursed items is that you cannot willingly get rid of them.  

#### Diary Entry of Day 1

**Minkolang** is awesome! Simple stack machine, makes a bit difficult to store variables, but that works well enough by swapping the top of the stack around.

Only the [web interpreter](http://play.starmaninnovations.com/minkolang/?code=) works correctly, the hello world examples of the python version print broken things.

Turns out there are in fact registers and even arrays, I just didn't realize they are there.

#### Diary Entry of Day 2

```LOLCODE
HOW IZ I CODIN, VISIBLE "I HAZ ALREADY OPEN IZZUE", GTFO
IF U SAY SO
```

String operations only exist in version 2.0 of **LOLCODE**, which is unofficial but also runs on the cray supercomputer and is highly parallelizable. Didn't get that to run

I didn't realize beforehand that segfaults were supposed to happen, but I guess that's fine. [LIEK](https://github.com/justinmeza/lci/issues/65) is broken, but what did I expect.

#### Diary Entry of Day 3 and not yet a half

I expected **COBOL** to be full of ancient secrets and dark magic. And indeed, magic awaited me. The initial impression of assembly that the few code samples I found in the modern global weave made were a ruse. 

It's not just a crumbly scroll, that scroll is alive! Whereas modern languages lack quick-and-deep-dive-tutorials, COBOL has a 100-page [Quick Guide](https://open-cobol.sourceforge.io/guides/GnuCOBOL%202.2%20OCT2017%20Quick%20Reference%20(A4).pdf) for the wizards that don't need the [2017 Programmer's Guide](https://open-cobol.sourceforge.io/HTML/gnucobpg.html) with the full 1000 pages that still somehow lacks useful examples. (In case you need it, check out my [Hello World](./2019/3/test.cob)). So of course I reached out into the weave's collection of the hive's ancient knowledge.

The hivemind answered my prayers: "That should be outlined in the fine manual".

In order to find the answers in the tome, you need to already know the spell though. So the goal that had been to create art turned into a quest for survival.

> Coding COBOL at 174km/h on foreign territory on new year's first day. Living the l337 live!

> TIL: COBOL-CASE is a thing and it's also called TRAIN-CASE 

By the Way, COBOL is sentient, and it thinks it is better than java. But even the examples it gives itself show that it's not.

#### D̰̣͓͍̮̘̹͡i̮̥̟ͅa͙̝̪̬͞r̸y͕̞̭̠ ̷̙͙E̻̟̫͓͈̦ͅn̸͓̩̯̜̣̞̦t̢̯̣̗̪ͅr͏̪y̫̹̼͙̣ ̰͕̱̻ợf̨̠̝̮̣͕̘͕ ̡̤̹̬D͓͚̖͍̼̣ͅa͏y̡̫͚̰̙ ̩̫͎͘4̖̥̦̻̖̤

We planned on using **legit**. There's a problem with that, though: Legit only treats numbers as numbers when they are hardcoded - otherwise, they're ascii.

Also, the intially compelling weirdness soon became boring. Legit is based on the commit tree instead of the file contents - that's cool. But in order to write useful code, you pretty much need to write another script that generates you your legit tree.

So we instead tried **chef**. See [https://github.com/lucidBrot/adventofcode/blob/master/2019/4/pt1.chef](this file) for an example that outputs numbers from 211 to 400 and divides each by 10 as well. It reads like a recipe - somewhat.

However, it turns out that there is neither a way to round floats, nor a way to check for `a < b`. The only comparison available is `a == 0`. So to ascertain that digit `a` is smaller or equal to digit `b`, one has to loop through every digit possible and check whether they are equal. Hardcoding a 6-fold nested loop for that is possible, but not enticing.

So we decided to use **LISP** instead. At least it's a programming language that has specs I think.# 

#### Diary Entry of Day $5

Choosing a disputably usable language now, we wrote the latest IntCode Computer using **php**.

It was less painful than expected, as the weirdnesses resolved quickly. I think it's actually a good thing that my global variables were deemed undefined within the functions. Only this here tripped me up:

> Parse error: Invalid numeric literal in ... on line **23**
>
> ```php
> 20 ...
> 21    case 07:
> 22      return 2;
> 23    case 08:
> 24      return 2;
> 25 ...
> ```

The issue is found quickly though: php interprets any number literal (thank goodness it's only literals!) starting with `0` as provided in octal base. Hence, `08` is not valid and would actually need to be written as `010`  - or simply as `8`.

#### Diary Entry of Day 6

**Bash**. It's weirder than expected.

All variables are by default global, even when defined within a function. Functions always return an integer error code, but you can have them echo to `stdout` and capture the output. Variables need quotes everywhere to avoid them splitting in two words, and `set -x` outputs the lines to be executed with already replaced variables.

Comparing integers using `(( a < b ))` is okay but don't do that with strings.![image-20200412143539833](.\2019\6\image-20200412143539833.png)

[shellcheck.net](shellcheck.net) is nice but only catches half of my mistakes.