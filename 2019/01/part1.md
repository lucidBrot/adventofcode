## Minkolang
```Minkolang
$n0($r3:2-+I1-)N.
```

`$n` reads all numbers into the stack

`0` push 0 on to the stack as the base element for the fold

`(` something like goto

`$r` swap the top two stack elements so that the sum-until-now is now the second topmost element

`3:` divide the top of the stack by 3

`2-` subtract 2 from the top of the stack

`+` add the result of this number to the sum. The sum is now again on top of the stack.

`I` get the length of the stack, `1-` subtract one from it. It is now 0 iff the stack only contains one element apart from the stack length

`)` jump back to `(` if the topmost value is truthy, otherwise continue

`N` print the topmost number as a number

`.` End program

```
output: 3329926
```

