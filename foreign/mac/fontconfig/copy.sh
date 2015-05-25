#! /bin/sh

if [ -d include ]; then
  rm -r include
fi

if [ -d lib ]; then
  rm -r lib
fi

mkdir include
cp -r /Users/cartier/Devel/local/include/fontconfig include/fontconfig

mkdir lib
cp /Users/cartier/Devel/local/lib/libfontconfig.1.dylib lib/libfontconfig.1.dylib
chmod 755 lib/libfontconfig.1.dylib
