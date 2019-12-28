### First attempt
```Minkolang
n02F       /}
           #>x$+N.
1{d3:2-d0`?^v
            {
```

This operates on one mass input only.  
The task says that we should compute it for each module instead. Didn't see that at first.

### Second attempt
Do that for all module inputs.
Though... this still considers the masses themselves too though.
```
n02F       /}
           #>x$+nd1+?v02F
1{d3:2-d0`?^v        x
            {        >N.
```

### Third attempt
```
n##3:2-02F

1{(d3:2-d0`)x$+}nd1+tx$+N.t02Ft
```

### Fourth attempt
```Minkolang
n3:2-02F

1{(d3:2-d0`)x$+}nd1+txx$+N.t10Ft
```

Works well with `130, 130`, and `1969, 1969` but fails with `100756`.

### Fifth attempt
```Minkolang
n3:2-02F 
           
1{(d3:2-d0`)x$+}nd1+5&x$+N.10F
```
Works but is ugly

## Sixth attempt

```Minkolang
n3:2-1{(d3:2-d0`)x$+}nd1+5&x$+N.10F
```

