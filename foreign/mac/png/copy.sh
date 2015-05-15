#! /bin/sh

if [ -d lib ]; then
  rm -r lib
fi

mkdir lib
cp /usr/local/Cellar/libpng/1.6.17/lib/libpng16.16.dylib lib
chmod 755 lib/libpng16.16.dylib
