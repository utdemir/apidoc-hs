# apidoc-hs

[![Build Status](https://travis-ci.org/utdemir/apidoc-hs.svg?branch=master)](https://travis-ci.org/utdemir/apidoc-hs)

Generate Haskell data types from [Apidoc](http://apidoc.me/doc/) schemas using Template Haskell.

## Exports

* **Apidoc.Types**
  * FromJSON and ToJSON instances for [Apidoc spec's](http://apidoc.me/bryzek/apidoc-spec/latest)

* **Apidoc.TH**
  * Read service.json from a local file or remote url on compile time
  * Generate
    * Haskell types for models, unions and enums
    * ToJSON, FromJSON instances
    * Lenses using [microlens](https://hackage.haskell.org/package/microlens) package

## Usage

See [Apidoc.Types](https://github.com/utdemir/apidoc-hs/blob/master/src/Apidoc/Types.hs) module for usage and it's respective [Haddock](https://utdemir.github.io/apidoc-hs/apidoc-hs-0.0.1/Apidoc-Types.html) for the generated code.

## TODO

* Dependency resolution of schemas
* Better handling of name clashes
* Prism's and Traversal's
* Servant API definition for resources
