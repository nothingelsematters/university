### Windows compilation process

***

### Linux compilation process

**fPIC** -> Position Independent Code

compilation command, code isn't bind to an exact address

**shared** -> output is .so, not an executable file

linker command

by default .dll's aren't being searched in an exact catalog,
therefore:

**LD_LIBRARY_PATH =** ...

If there are two functions one in a static library and another one
in user program, the last one is preferred, so it is possible to
"redefine" some of the standard functions (e.g. malloc).
Main program functions have priority over shared objects'

In PIC it is **not** possible to inline functions, because there
is a probability, that someone will want to redefine that function

in the following code w/o -fPIC compiler will inline f, w/ - can't

```cpp
int f() {
    return 42;
}

int g() {
    return f();
}
```

#### **interposition** :

> e.g. If there's `main()`, that calls `foo()` from the lib and
`bar()` in the main file; and `foo()`, that calls `bar()` in
the library, it goes:

```
> main() calls foo() from library
> foo() calls main's bar()
```

In order not to let a function be seen out of ur program, u can
put it in a namespace; but u can mark it **inline**, that means,
that if function w/ the same declaration exists, its body will
be the same


U can write a function w/ `_attribute_(visibility("hidden"))`
(almost the same string w/ gnu) and  it won't be seen out of
ur program

It is possible to give a compiler in a command line flag
`-fvisibility(hidden)`

#### **lazy binding**

// opposite: LD_BIND_NOW: to bind all the symbols right now
