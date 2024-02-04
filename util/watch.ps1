function Get-Table($folder, $ignore = @()) {
  function Get-Structure($dir) {
    $buffer = ""
    Get-ChildItem $dir -Directory | ForEach-Object {
      if (!$ignore.Contains($_.Name)) {
        $buffer += "<$($_.Name)>"
        $buffer += "$(Get-Structure $_)"
      }
    }
    Get-ChildItem $dir -File | ForEach-Object {
      if (!$ignore.Contains($_.Name)) {
        $path = (Resolve-Path $_ -Relative).TrimStart(".") `
          -replace ("\\", "/") `
          -replace (" ", "%20")
        $buffer += "<[$($_.Name)]($((Get-FileHash $_).Hash))>"
      }
    }
    return "{$buffer}"
  }

  $indentCounter = -1
  $buffer = "`n"
  (Get-Structure $folder) -split '' | ForEach-Object {
    switch ($_) {
      "{" { $indentCounter++; break }
      "}" { $indentCounter--; break }
      "<" { $buffer += "$("    " * $indentCounter)- "; break }
      ">" { $buffer += "`n"; break }
      Default { $buffer += $_; break }
    }
  }
  return $buffer
}

Write-Output "[ $(Get-Date -f "hh:mm:ss") ] ( `e[36mStarted Watching`e[0m )"
while ($true) {
  $a = Get-Table ".\src"
  Start-Sleep -Seconds 1
  $b = Get-Table ".\src"
  if ($a -ne $b) {
    Write-Output "[ $(Get-Date -f "hh:mm:ss") ] ( `e[32mBuild Triggered`e[0m )"
    cabal.exe run --verbose=0
    Write-Output "[ $(Get-Date -f "hh:mm:ss") ] ( `e[33mBuild Ended with $LASTEXITCODE`e[0m )"

  }
}
