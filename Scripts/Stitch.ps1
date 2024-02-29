function getFileContent ($param) {
  $a = @"
samples: 50
height:  270
width:   480
camera:
  cameraOrigin: [0, 1.2, -1.3]
  lookingAt:    [$($param), 1, 0]
  focalLength:  1
  fov:          1.57
  cameraUpVec:  [0, 1, 0]
  defocusAngle: 0.08
"@
  return $a
}

$frame = 1
for ($i = -5; $i -lt 5; $i+=0.1) {
  New-Item ".\temp\$($frame).yaml"
  $c = getFileContent($i)
  Add-Content -Path ".\temp\$($frame).yaml" -Value $c
  $frame++
}

Get-ChildItem "./temp/" | ForEach-Object -Parallel {
  .\dist-newstyle\build\x86_64-windows\ghc-9.4.8\Lime-0.1.0.0\x\LimeExe\build\LimeExe\LimeExe.exe -i ".\temp\$($_.BaseName).yaml" -o ".\temp\$($_.BaseName).png"
}
