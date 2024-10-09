#!/bin/bash

filename=$1
# extract filename without extension
filename_no_ext="${filename%.*}"

ffmpeg -i $filename -vf "fps=30,scale=512:-1:flags=lanczos" $filename_no_ext.gif