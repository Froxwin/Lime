#!/usr/bin/env bash

printf "pre-commit: \e[33mRunning formatters...\e[0m\n"
find ./Source/ ./Test/ ./Benchmark/ -type f | xargs -I {} -P 20 bash -c 'fourmolu "{}" -i -q && stylish-haskell "{}" -i -r' --null
cabal-fmt ./Lime.cabal --inplace

printf "pre-commit: \e[33mRunning hlint...\e[0m\n"
if ! hlint ./Source -q ; then
  printf "pre-commit: \e[31mAborting commit as hlint returned non-empty\e[0m\n"
  exit
fi

printf "pre-commit: \e[33mRunning tests...\e[0m\n"
if ! cabal test --test-show-details=failures --verbose=0 ; then
  printf "pre-commit: \e[31mAborting commit as some tests failed\e[0m\n"
  exit
fi

printf "pre-commit: \e[33mRunning build...\e[0m\n"
if ! cabal build --ghc-options=-Werror --verbose=0 ; then
  printf "pre-commit: \e[31mAborting commit as build threw errors\e[0m\n"
  exit
fi

printf "pre-commit: \e[1;32mFinished\e[0m\n"
exit
