# WYaScheme

This is my attempt following [Write yourself a Scheme in 48 hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

## Getting started

I am using [`stack`](https://docs.haskellstack.org/en/stable/#the-haskell-tool-stack) instead of `calbal-install`,
even though `cabal` itself is supposed to have resolved hell issues as of 2025.
The goal here is to follow all recommendations for newcomers.

1. Install [`ghcup`](https://www.haskell.org/ghcup/), the main installer for Haskell.
   It provides a tool that can be used to install Stack, GHC, HLS etc.
   HLS is a program that is used by Haskell extensions for popular code editors.
   By default, the script to install GHCup also configures Stack so that if Stack needs a version of GHC, GHCup takes over obtaining and installing that version.
2. Run

   ```shell
   stack build
   stack run
   ```
