#!/bin/bash
export PATH="$PATH:/bin:/sbin:/usr/sbin"
/bin/schroot -c "$1" -- "$2"
