---
title: Installation
---

Install using [abapGit](http://abapgit.org), or copy the source code manually into your system. Installing via abapGit requires an empty package to be created eg. $AOC

## SCI setup
- Activate checks in transaction SCI (depending on release)
  - Goto -> Management of -> Tests
  - Code Inspector -> Management of -> Tests
  - Code Inspector -> Management of -> Checks 
- Configure check variant

## Updating
Backup variants using [upDOWNci](https://github.com/larshp/upDOWNci).

Run abapGit, click "pull". Note that the option will only show up if new code has been added to the repository.

Or click "import zip" if using the offline project feature in abapGit.

## Uninstall
Run abapGit, click "uninstall".
