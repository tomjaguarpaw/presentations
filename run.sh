clear && ghc -fforce-recomp -Wall -Werror Main.hs && ./Main > testOut <<EOF
Bob
Stan
Doris

Jones
EOF

diff -u expectedOut testOut
