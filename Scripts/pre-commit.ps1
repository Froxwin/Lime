#!/usr/bin/env pwsh

Write-Output "pre-commit: Running brittany..."
Get-ChildItem .\Source -Filter *.hs | ForEach-Object { brittany $_ --write-mode=inplace }

Write-Output "pre-commit: Running hlint..."
$nHints = (hlint .\Source\) | rg "hint*"
if ($nHints -ne "No hints") {
  throw "pre-commit: Aborting commit as hlint returned non-empty :: $(($nHints))"
}

Write-Output "pre-commit: Runnnig tests..."
cabal test --test-show-details=failures --verbose=0
if (!$?) { throw "pre-commit: Aborting commit as some tests failed" }
Write-Output "pre-commit: `e[32mFinished`e[0m"
Exit