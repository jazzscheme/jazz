#! /bin/sh

if [ -d include ]; then
  rm -r include
fi

if [ -d lib ]; then
  rm -r lib
fi

mkdir include
cp -r /Users/cartier/Devel/local/include/cairo include

mkdir lib
cp /Users/cartier/Devel/local/lib/libcairo.2.dylib lib/libcairo.2.dylib
chmod 755 lib/libcairo.2.dylib
