# react-intl-auto-id-ppx

[![Actions Status](https://github.com/SimonDegraeve/react-intl-auto-id-ppx/workflows/CI/badge.svg)](https://github.com/SimonDegraeve/react-intl-auto-id-ppx/actions)
[![NPM Version](https://badge.fury.io/js/react-intl-auto-id-ppx.svg)](https://badge.fury.io/js/react-intl-auto-id-ppx)

Reason/OCaml PPX for generating [ReactIntl](https://github.com/formatjs/formatjs) `id` from `defaultMessage`.

## Installation

### With `yarn` or `npm` on Bucklescript projects

Install the PPX with `yarn` or `npm`

```bash
yarn add --dev react-intl-auto-id-ppx
# Or
npm install --dev react-intl-auto-id-ppx
```

And add the PPX in your `bsconfig.json` file:

```json
{
  "ppx-flags": ["react-intl-auto-id-ppx/react-intl-auto-id-ppx"]
}
```

## Developing

After cloning the repository, you should run both `esy` and `yarn` to install
dependencies. `react-intl-auto-id-ppx` uses `esy` for managing the important dependencies,
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

3. [Create detailed release notes](https://github.com/SimonDegraeve/react-intl-auto-id-ppx/releases) for the new version, following the `Added/Changed/Fixed/Removed` format. Note that the new version of the PPX will automatically be pushed to NPM and a release will be created on GitHub.

## Background/Sources

- [PPX Inspiration](https://github.com/dylanirlbeck/tailwind-ppx)
