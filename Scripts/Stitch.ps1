function getFileContent ($param) {
  $a = @"
samples: 20
height:  270
width:   480
camera:
  cameraOrigin: [0, 1, -2]
  lookingAt:    [0, 1, 150]
  focalLength:  1
  fov:          1.57
  cameraUpVec:  [0, 1, 0]
  defocusAngle: 0.07
world:
  - tag: Sphere
    center: [0, -100.5, 0]
    radius: 100
  - tag: Sphere
    center: [0, 1, 0]
    radius: 1
  - tag: Sphere
    center: [3, 1, $($param)]
    radius: 1
"@
  return $a
}

$frame = 1
for ($i = 4; $i -gt -1; $i-=0.1) {
  New-Item ".\temp\$($frame).yaml"
  $c = getFileContent($i)
  Add-Content -Path ".\temp\$($frame).yaml" -Value $c
  $frame++
}

Get-ChildItem "./temp/" | ForEach-Object -Parallel {
  .\dist-newstyle\build\x86_64-windows\ghc-9.4.8\Lime-0.1.0.0\x\Lime\build\Lime\Lime.exe -i ".\temp\$($_.BaseName).yaml" -o ".\temp\$($_.BaseName).png"
} -ThrottleLimit 10
