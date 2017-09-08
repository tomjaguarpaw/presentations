clear && ghc -fforce-recomp -Wall Main.hs && ./Main > testOut <<EOF
Bob!@#
Stan123
///Doris///

%Jones%
EOF

diff -u expectedOut testOut
