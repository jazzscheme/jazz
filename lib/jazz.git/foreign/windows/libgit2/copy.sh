#! /bin/sh

if [ -d include ]; then
  rm -r include
fi

if [ -d lib ]; then
  rm -r lib
fi

mkdir include
cp /c/Home/system/libgit2-1.1.0/include/git2.h include
cp -r /c/Home/system/libgit2-1.1.0/include/git2 include

mkdir lib
cp /c/Home/system/libgit2-1.1.0/build/Debug/git2.dll lib
