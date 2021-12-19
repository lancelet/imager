#!/usr/bin/env bash

find . -name '*.hs' -exec bash -xc 'brittany --write-mode=inplace {}' \;
