#! /bin/sh

if [ -d include ]; then
  rm -r include
fi

if [ -d lib ]; then
  rm -r lib
fi

mkdir include
cp -r /Users/cartier/Devel/local/include/soundio include/soundio

mkdir lib
cp /Users/cartier/Devel/local/lib/libsoundio.1.1.0.dylib lib/libsoundio.1.1.0.dylib
chmod 755 lib/libsoundio.1.1.0.dylib
