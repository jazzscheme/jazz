#! /bin/sh

if [ -d include ]; then
  rm -r include
fi

if [ -d lib ]; then
  rm -r lib
fi

cp -r /Users/cartier/Devel/local/include/freetype2 include

mkdir lib
cp /Users/cartier/Devel/local/lib/libfreetype.6.dylib lib/libfreetype.6.dylib
chmod 755 lib/libfreetype.6.dylib
