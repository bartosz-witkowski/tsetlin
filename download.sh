#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

MNIST=http://yann.lecun.com/exdb/mnist
FASHION=http://fashion-mnist.s3-website.eu-central-1.amazonaws.com

files=(\
  "train-images-idx3-ubyte.gz" \
  "train-labels-idx1-ubyte.gz" \
  "t10k-images-idx3-ubyte.gz" \
  "t10k-labels-idx1-ubyte.gz" \
)

mkdir mnist
mkdir fashionMnist

for f in "${files[@]}" ; do
  funzipped=$(basename $f .gz)

  wget "${MNIST}/${f}"
  gunzip $f
  mv ${funzipped} mnist/

  wget "${FASHION}/${f}"
  gunzip $f
  mv ${funzipped} fashionMnist/
done
