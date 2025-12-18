#!/usr/bin/env sh

in="$1"
out="${in%.*}.bloom.png"

magick "$in" \
    \( -clone 0 -channel RGB -separate +channel -evaluate-sequence max -auto-level -threshold 80% -negate -blur 0x5 -negate \) \
    -compose Screen -composite \
    \( -clone 0 -channel RGB -separate +channel -evaluate-sequence max -auto-level -threshold 80% -negate -blur 0x20 -negate \) \
    -compose Screen -composite \
    \( -clone 0 -channel RGB -separate +channel -evaluate-sequence max -auto-level -threshold 80% -negate -blur 0x65 -negate \) \
    -compose Screen -composite \
    \( -clone 0 -channel RGB -separate +channel -evaluate-sequence max -auto-level -threshold 80% -negate -blur 0x80 -negate \) \
    -compose Screen -composite \
    "$out"

echo "Bloom completed → $out"
