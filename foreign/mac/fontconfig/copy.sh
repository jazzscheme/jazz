#! /bin/sh

if [ -d include ]; then
  rm -r include
fi

if [ -d lib ]; then
  rm -r lib
fi

cp -r /usr/local/Cellar/fontconfig/2.11.1/include include

mkdir lib
cp /usr/local/Cellar/fontconfig/2.11.1/lib/libfontconfig.1.dylib lib/libfontconfig.1.dylib
chmod 755 lib/libfontconfig.1.dylib
