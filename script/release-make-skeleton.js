#!/usr/bin/env node

const { execSync } = require('child_process')
const fs = require('fs')
const path = require('path')
const esy = require('../esy.json')

console.log(`Using esy.json:\n${esy}`)

const filesToCopy = ['LICENSE', 'README.md']

function exec(cmd) {
  console.log(`exec: ${cmd}`)
  return execSync(cmd).toString()
}

function mkdirpSync(p) {
  if (fs.existsSync(p)) {
    return
  }
  mkdirpSync(path.dirname(p))
  fs.mkdirSync(p)
}

function removeSync(p) {
  exec(`rm -rf -i "${p}"`)
}

const src = path.resolve(path.join(__dirname, '..'))
const dst = path.resolve(path.join(__dirname, '..', '_release'))

mkdirpSync(dst)
removeSync(dst)
mkdirpSync(dst)

for (const file of filesToCopy) {
  const p = path.join(dst, file)
  mkdirpSync(path.dirname(p))
  fs.copyFileSync(path.join(src, file), p)
}

fs.copyFileSync(path.join(src, 'script', 'release-postinstall.js'), path.join(dst, 'postinstall.js'))

const filesToTouch = ['ppx.exe', 'ppx', 'extract.exe', 'extract']

for (const file of filesToTouch) {
  const p = path.join(dst, file)
  mkdirpSync(path.dirname(p))
  fs.writeFileSync(p, '')
}

const pkgJson = {
  name: 'res-react-intl',
  version: esy.version,
  description: esy.description,
  homepage: esy.homepage,
  license: esy.license,
  repository: esy.repository,
  keywords: esy.keywords,
  scripts: {
    postinstall: 'node postinstall.js',
  },
  bin: {
    'res-react-intl-ppx.exe': 'ppx.exe',
    'res-react-intl-extract.exe': 'extract.exe',
    'res-react-intl-ppx': 'ppx',
    'res-react-intl-extract': 'extract',
  },
  files: [
    'platform-windows-x64/',
    'platform-linux-x64/',
    'platform-darwin-x64/',
    'postinstall.js',
    'ppx.exe',
    'ppx',
    'extract.exe',
    'extract',
    'bsconfig.json',
  ],
}

fs.writeFileSync(path.join(dst, 'package.json'), JSON.stringify(pkgJson, null, 2))

const bsConfig = {
  name: pkgJson.name,
  'bs-dependencies': ['reason-react'],
  reason: {
    'react-jsx': 3,
  },
  refmt: 3,
  'package-specs': {
    module: 'es6',
    'in-source': true,
  },
  suffix: '.bs.js',
}

fs.writeFileSync(path.join(dst, 'bsconfig.json'), JSON.stringify(bsConfig, null, 2))
