#!/bin/bash

exists() {
    which "${1}" > /dev/null
}

if exists runhaskell; then
    runhaskell ${@}
elif exists runghc; then
    runghc ${@}
elif exists runhugs; then
    runhugs ${@}
else
    echo "You need runhaskell, runghc, or runhugs to build this program."
    exit 1
fi
