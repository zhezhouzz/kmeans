#!/bin/bash

echo $1
echo $2

./compiler/compiler $2 > $2".sml"
cat dep.mlb > tmp.mlb
echo $1 >> tmp.mlb
echo $2".sml" >> tmp.mlb
mlton -default-ann 'allowRecordPunExps true' tmp.mlb
./tmp
