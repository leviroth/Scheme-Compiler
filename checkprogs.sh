#!/usr/bin/env bash


for prog in $*; do
    ./_build/default/compiler.exe < ./$prog
done
