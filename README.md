# shx

## Summary

SHX is a toy compiler from an S-expression language to shell scripts. It is
implemented in Emacs Lisp.

- Shell features: pipes, variables, quoting
- Flow control constructs: `if`, `unless`, `when`, `cond`

Here's a chunk of SHX:

```lisp
;; Setting and exporting variables
(set!    X 20)
(export! Y 21)

;; Infix operators

[X < Y] ; Is equivalent to:
(< X Y)

;; Flow control

(if [X < Y]
    (echo "math works")
  (echo "lolwut"))

(cond ((zero? X) (echo "X is 0"))
      ([X = 20]  (echo "X is 20"))
      ([X = 21]  (echo "X is 21"))
      (otherwise (echo X)))

;; Pipes

(-> "ps -ef" "grep -i emacs" "xargs echo")

;; Boolean logic

(and (dir-exists? "~/Desktop")
     (f-writable? "~/Desktop")
     (not (f-exists? "~/Deskop.lock")))
```

Here's the shell script it will compile to:

```sh
X=20
export Y=21

[ ${X} -lt ${Y} ];
[ ${X} -lt ${Y} ];

if [ ${X} -lt ${Y} ]; then
    echo math works
else
    echo lolwut
fi

if [ ${X} -eq 0 ]; then
    echo X is 0
elif [ ${X} -eq 20 ]; then
    echo X is 20
elif [ ${X} -eq 21 ]; then
    echo X is 21
else
    echo ${X}
fi

ps -ef | grep -i emacs | xargs echo

[ -d ~/Desktop ] && [ -w ~/Desktop ] && [ ! -f ~/Deskop.lock ];
```

## Installing

You will need Emacs 24+, `make` and [Cask](https://github.com/cask/cask) to
build the project.

    cd shx
    make && make install

## Contributing

Yes, please do! See CONTRIBUTING.md for guidelines.

## License

See COPYING. Copyright (c) 2014 Chris Barrett.
