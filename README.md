# json-litobj [![Build Status](https://travis-ci.org/jonathankochems/json-litobj.svg)](https://travis-ci.org/jonathankochems/json-litobj) [![codecov.io](http://codecov.io/github/jonathankochems/json-litobj/coverage.svg?branch=develop)](http://codecov.io/github/jonathankochems/json-litobj?branch=develop)

This module extends Text.JSON to enable the decoding of strings containing literal JS objects.
In particular, it relaxes the restrictions that fields in JSON objects must be strings.

For example:

* JSON conformant:  

> { "foo" : "bar" }

* literal JS object: 

> { foo : "bar" }

## Documentation

The haddock documentation can be found on [hackage](https://hackage.haskell.org/package/json-litobj-0.1.0.0/candidate).  