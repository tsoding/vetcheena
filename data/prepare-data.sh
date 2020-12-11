#!/bin/sh

set -e

mkdir -p train/ham
mkdir -p train/spam
mkdir -p validate/ham
mkdir -p validate/spam

for i in `seq 1 6`; do
    7z x ./enron$i.7z 
    cp -rv ./enron$i/ham train/
    cp -rv ./enron$i/spam train/
    rm -rv ./enron$i    
done

for i in `ls ./train/ham | head -n 100`; do
    mv -v "./train/ham/$i" "./validate/ham/"
done

for i in `ls ./train/spam | head -n 100`; do
    mv -v "./train/spam/$i" "./validate/spam/"
done
