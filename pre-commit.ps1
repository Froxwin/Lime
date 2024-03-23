#!/bin/pwsh

Write-Output "Running brittany..."
Get-ChildItem .\Source -Filter *.hs | ForEach-Object { brittany $_ --write-mode=inplace }

Write-Output "Running hlint..."
$nHints = (hlint .\Source\) | rg "hint*"
if ($nHints -ne "No hints") { throw "Hlint returned non-empty :: $(($nHints))" }

Write-Output "Runnnig tests..."
$testOut = (cabal test --test-show-details=failures) | rg "failed"
if ([bool]$testOut) { throw "Some tests failed :: $(($testOut))" }

Write-Output "`e[32mFinished`e[0m"
