#!/usr/bin/env sh

find ./Source/ ./Lime/ ./Test/ ./Benchmark/ -type f | xargs -I {} -P 20 bash -c 'fourmolu "{}" -i -q' --null
cabal-fmt ./Lime.cabal --inplace
printf "\e[1;32mFinished Formatting\e[0m\n"
