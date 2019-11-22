#!/bin/bash

set -e
set -x

mkdir test-repo
cd test-repo
git init .
touch HELLO
git add HELLO
git commit -m"hello"


get_files(){
  local version=${1}
  shift
  for f in pdb.py tempfile.py os.py inspect.py mimetypes.py; do
    curl -o "${f}" "https://raw.githubusercontent.com/python/cpython/${version}/Lib/${f}"
  done
  git add -A
  git commit -m"version ${v}"
}

git checkout master
git checkout -b v1
get_files v3.4.8
git checkout -b v2
git checkout -b v3
git checkout v1
get_files v3.5.0
get_files v3.6.0
git checkout v2
get_files v3.7.0
git checkout v3
get_files v3.8.0
git checkout master
git checkout v2
