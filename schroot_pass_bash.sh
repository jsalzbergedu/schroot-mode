#!/bin/bash
export PATH="$PATH:/bin:/sbin:/usr/sbin"
schroot -c "$1" -- /bin/bash -c "$2"
