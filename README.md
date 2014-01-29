# shx

## Summary

SHX is a toy compiler from an S-expression language to shell scripts. It is
implemented in Emacs Lisp.

- Shell features: pipes, variables, quoting
- Flow control constructs: `if`, `unless`, `when`, `cond`
- Shell features: pipes, variables, quoting

Here's a chunk of SHX:

```lisp
(set! EXAMPLE 20)

(cond ((= EXAMPLE 20)
       (echo "success"))
      ((f-exists? "~/Desktop/hello")
       (set! STR ($ ls "-la"))
       (echo "works"))
      (t
       (echo EXAMPLE)))
```

Here's the shell script it will compile to:

```sh
EXAMPLE=20

if [ ${EXAMPLE} -eq 20 ]; then
    echo success
elif [ -f ~/Desktop/hello ]; then
    STR=$(ls -la)
    echo works
else
    echo ${EXAMPLE}
fi
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
