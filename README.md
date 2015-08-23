# json-litobj [![Build Status](https://travis-ci.org/jonathankochems/json-litobj.svg)](https://travis-ci.org/jonathankochems/json-litobj) [![codecov.io](http://codecov.io/github/jonathankochems/json-litobj/coverage.svg?branch=develop)](http://codecov.io/github/jonathankochems/json-litobj?branch=develop) [![Available on Hackage][badge-hackage]][hackage] [![License BSD3][badge-license]][license]

This module extends Text.JSON to enable the decoding of strings containing literal JS objects.
In particular, it relaxes the restrictions that fields in JSON objects must be strings.

For example:

* JSON conformant:  

> { "foo" : "bar" }

* literal JS object: 

> { foo : "bar" }

## Documentation

The haddock documentation can be found on [hackage](https://hackage.haskell.org/package/json-litobj-0.1.0.0/candidate).

## Motivation

I wanted to parse JSON responses from various websites with Text.JSON. Unfortunately, I ran into parsing errors due to literal JS objects included in the answer strings. Since literal JS object are not really part of the JSON format I started this module to work around this problem.

## Contributing

If you feel that this module is missing something useful which should be part of a more ``permissive'' JSON parsing please consider a contribution.

To contribute:

1. fork this repository
2. create a feature branch 
3. commit and push your code to your feature branch
4. create a pull request to this repository

