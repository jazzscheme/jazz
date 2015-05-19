#! /bin/sh

if [ -d include ]; then
  rm -r include
fi

if [ -d lib ]; then
  rm -r lib
fi

cp -r /usr/local/Cellar/pixman/0.32.6/include include

mkdir lib
cp /usr/local/Cellar/pixman/0.32.6/lib/libpixman-1.0.dylib lib
chmod 755 lib/libpixman-1.0.dylib
