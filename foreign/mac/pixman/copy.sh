#! /bin/sh

if [ -d lib ]; then
  rm -r lib
fi

mkdir lib
cp /usr/local/Cellar/pixman/0.32.6/lib/libpixman-1.0.dylib lib
chmod 755 lib/libpixman-1.0.dylib
