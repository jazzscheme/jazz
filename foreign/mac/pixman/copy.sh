#! /bin/sh

if [ -d include ]; then
  rm -r include
fi

if [ -d lib ]; then
  rm -r lib
fi

mkdir include
cp -r /Users/cartier/Devel/local/include/pixman-1 include/pixman-1

mkdir lib
cp /Users/cartier/Devel/local/lib/libpixman-1.0.dylib lib
chmod 755 lib/libpixman-1.0.dylib
