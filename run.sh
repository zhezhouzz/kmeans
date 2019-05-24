#!/bin/bash

mkdir -p "build/"$(dirname "$2")
touch "build/"$2".sml" || exit
./compiler/compiler $2 > "./build/"$2".sml"
touch "build/tmp.mlb" || exit
cat dep.mlb > build/tmp.mlb
echo "../"$1 >> build/tmp.mlb
echo $2".sml" >> build/tmp.mlb
mlton -default-ann 'allowRecordPunExps true' build/tmp.mlb
./build/tmp
