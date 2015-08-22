# json-litobj [![Build Status](https://travis-ci.org/jonathankochems/json-litobj.svg)](https://travis-ci.org/jonathankochems/json-litobj)

This module extends Text.JSON to enable the decoding of strings containing literal JS objects.
In particular, it relaxes the restrictions that fields in JSON objects must be strings.

For example:

* JSON conformant:  
> { "foo" : "bar" }
* literal JS object: 
> { foo : "bar" }