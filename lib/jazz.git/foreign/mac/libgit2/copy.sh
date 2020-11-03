#! /bin/sh

if [ -d include ]; then
  rm -r include
fi

if [ -d lib ]; then
  rm -r lib
fi

cp -r /Users/cartier/Devel/system/libgit2-1.1.0/include include

mkdir lib
cp /Users/cartier/Devel/system/libgit2-1.1.0/build/libgit2.1.1.0.dylib lib
