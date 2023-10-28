# Patched `stm` package for debugging

## Features

* STM transactions will include a stacktrace
  (no more endless hunting for where a "blocked indefinitely" exception is
  coming from)
* `TVar` and `TMVar` have a `Show` instance that can be used to used to
  identity and distinguish variables.

## Usage

Add the following to your `cabal.project` file:

```
source-repository-package
  type: git
  location: https://github.com/edsko/stm-debug
  tag: edsko/instrument-for-debugging
  subdir: . assignid patched/async patched/tasty/core patched/unliftio/unliftio

allow-newer: stm
```
