#!/bin/bash
# working finally

alias quick_git='function _gitgo() { git add . && git commit -m "$1" && git push; }; _gitgo'