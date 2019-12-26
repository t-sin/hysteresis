# Hysteresis

*Hysteresis* provides historized bindings; each symbols in the bindings hold past values.

*This library is in early stage of development*.

## Install

Clone this repository in your quicklisp path.

## Motivation

*To be wrote when I filled up with fine power*.

a function called repeatedly and it's possibly failure, reverting history in restart seems to be useful,  like game development.

## Usage

First, *hysteresis* has two core concept: *historized symbols* and *history*. *Historized symbols* are like symbols but they has its *history*. Hysteresis has a mapping from normal symbols to *Historized symbols* as a hash table `hysteresis::*symbols*`.

### Set/Get value

We can set/get value to/from historized symbols. To set a value, do like this:

```lisp
CL-USER> (hysteresis:set-value 'foo 42)
42
#(HYSTERESIS:HISTORIZED-SYMBOL ...)
```

Secound output of `hysteresis:set-value` is exactly *historized-symbol* object. To get this value from name, do like below:

```lisp
CL-USER> (hysteresis:get-value 'foo)
42
```

However it's complicated way. So I wrote a macro for accessing value slot of *hystorized symbols*. It defines symbol macro for specified name. When you remove this symbol macro, just `unintern` that symbol.

```lisp
CL-USER> (hysteresis:hsetq hoge 42)
42
CL-USER> (setf hoge 42)
42
CL-USER> hoge
42
```

### Get back the values at the past point

The difference between normal symbols and *historized symbols* is the one; *historized symbols* remember past values. The sequences of past values is called *history* in this library.

See a example. In this case I set values three times but now I want first value suddenly.

```lisp
;; set some value (it might be used by music player library)
CL-USER> (setf @song "Walking In The Sun")
"Walking In The Sun"
CL-USER> (setf @song "My Hero")
"My Hero"
CL-USER> (setf @song "Half The Man")
"Half The Man"
```

So I revert the history.

```lisp
CL-USER> @song
"Half The Man"
CL-USER> (hysteresis:revert 2)
NIL
CL-USER> @song
"Walking In The Sun"
```

Unfortunately now I want the most resent suddenly. Let's get back to the present.

```lisp
CL-USER> (hysteresis:present)
NIL
CL-USER> @song
"Half The Man"
```

By applying optional argument `name` as a symbol to `hysteresis:revert` and `hysteresis:present`, history reverting occur only one historized symbol identified by `name`.

### Historized functions

History entries in hystorized symbols have few slots; one of that is `value` and other one is `function`. Yes. We can define and hold functions defined at past. A macro `hysteresis:hdefun` is useful.

```lisp
CL-USER> (hysteresis:hdefun foo (x)
           x)
#<COMPILED-LEXICAL-CLOSURE #x302000DB1C8F>
CL-USER> (foo 42)
42
CL-USER> (foo 'hoge)
HOGE
CL-USER> (foo "wow")
"wow"
```

Functions defined by `hysteresis:hdefun` performs as same as normal functions. Core idea is they are historized, off cource they can be reverted. See this example:

```lisp
CL-USER> (hysteresis:hdefun foo (x)
           (list x x))
#<COMPILED-LEXICAL-CLOSURE #x302000E7309F>
CL-USER> (hysteresis:hdefun foo (x)
           (format nil "+~a+" x))
#<COMPILED-LEXICAL-CLOSURE #x302000ED2D6F>
CL-USER> (foo 'hoge)
"+HOGE+"
CL-USER> (hysteresis:revert 1)
NIL
CL-USER> (foo 'hoge)
(HOGE HOGE)
```

It's all of *hysteresis*.

## Author

- TANAKA Shinichi <shinichi.tanaka45@gmail.com>

## License

*Hysteresis* is licensed unther the MIT license.