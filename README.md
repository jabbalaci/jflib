# jflib - Jabba's Fortran Library

A utility library for modern Fortran.

Coming from Python, I missed a lot of things in Fortran.
Especially the string handling was painful.
This library tries to make coding in Fortran easier.

This is a work in progress.

## Contents

Features at a glance:

* assertions (`assert`, `assert_true`, `assert_false`)
* constants (`PI`, `WHITESPACE`, `DIGITS`)
* type conversions (`to_str()`)
* lots of string methods (`islower`, `isdigit`, `lower`, `find`, `strip`, `startswith`,
  `replace`, `split`, ...)
* StringBuffer, for storing variable-length strings
* simpler command-line arguments (`argv(i)` returns the $i^{th}$ argument)
* types (`dp`, etc.)

## Examples

Tests can be found in the `test/` folder. I also added some
examples in the `example/` directory.

## Related Work

* [stdlib](https://github.com/fortran-lang/stdlib) (it's a big one)
* [Fortran Utilities](https://github.com/certik/Fortran-utils) (which served as an inspiration to
  make my own library)
