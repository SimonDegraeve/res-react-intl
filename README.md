# res-react-intl

[![Actions Status](https://github.com/SimonDegraeve/res-react-intl/workflows/res-react-intl%20pipeline/badge.svg)](https://github.com/SimonDegraeve/res-react-intl/actions)
[![NPM Version](https://badge.fury.io/js/res-react-intl.svg)](https://badge.fury.io/js/res-react-intl)

Reason/OCaml PPX generating [ReactIntl](https://github.com/formatjs/formatjs) `id` _(short MD5)_ from `defaultMessage`.

This package provides also bindings overrides from [bs-react-intl](https://github.com/reasonml-community/bs-react-intl).

## Example

Input:

```re
// Demo.re
open ReactIntl;

module Msg = {
  [@intl.messages];
  let hello = {defaultMessage: "Hello"};
};

[@react.component]
let make = () => {
  <FormattedMessage defaultMessage="Some default message" />;
};
```

Output:

```js
// Demo.bs.js
// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict'

var React = require('react')
var ReactIntl = require('react-intl')

var Msg = {
  hello: {
    id: '_8b1a9953',
    defaultMessage: 'Hello',
  },
}

function Demo(Props) {
  return React.createElement(ReactIntl.FormattedMessage, {
    id: '_0beb880a',
    defaultMessage: 'Some default message',
  })
}

var make = Demo

exports.Msg = Msg
exports.make = make
```

## Installation

### With `yarn` or `npm` on Bucklescript projects

Install the PPX with `yarn` or `npm`

```bash
yarn add res-react-intl
# Or
npm install res-react-intl
```

And add the PPX in your `bsconfig.json` file:

```json
{
  "bs-dependencies": ["res-react-intl"],
  "ppx-flags": ["res-react-intl/res-react-intl"]
}
```

## Developing

After cloning the repository, you should run both `esy` and `yarn` to install
dependencies. `res-react-intl` uses `esy` for managing the important dependencies,
and `yarn` is used solely for pre-commit linting/formatting of Reason files.

### Relevant commands

- `esy build` -> Builds the project
- `esy watch` -> Watches for changes to Reason/OCaml files in the entire project, including in the `/test` directory
- `esy test_native` -> Runs the native tests (in `test/native`)
- `esy test_bs` -> Runs the BuckleScript tests (in `test/bucklescript`)

### Releasing

1. Bump the version of the ppx in `esy.json` on `master` (we use [semantic versioning](https://semver.org/))
2. Create and push a new tag

```
$ git checkout master
$ git tag vx.y.z
$ git push origin vx.y.z
```

3. [Create detailed release notes](https://github.com/SimonDegraeve/res-react-intl/releases) for the new version, following the `Added/Changed/Fixed/Removed` format. Note that the new version of the PPX will automatically be pushed to NPM and a release will be created on GitHub.

## Background/Sources

- [Setup PPX Inspiration](https://github.com/dylanirlbeck/tailwind-ppx)
