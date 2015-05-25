#! /bin/sh

if [ -d include ]; then
  rm -r include
fi

if [ -d lib ]; then
  rm -r lib
fi

cp -r /Users/cartier/Devel/local/include/libpng16 include

mkdir lib
cp /Users/cartier/Devel/local/lib/libpng16.16.dylib lib
chmod 755 lib/libpng16.16.dylib
