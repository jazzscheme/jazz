#! /bin/sh

if [ -d include ]; then
  rm -r include
fi

if [ -d lib ]; then
  rm -r lib
fi

cp -r /usr/local/Cellar/cairo/1.14.2/include include

mkdir lib
cp /usr/local/Cellar/cairo/1.14.2/lib/libcairo.2.dylib lib/libcairo.2.dylib
chmod 755 lib/libcairo.2.dylib
