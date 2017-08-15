#! /bin/sh

if [ -d include ]; then
  rm -r include
fi

if [ -d lib ]; then
  rm -r lib
fi

mkdir include
cp -r /usr/local/opt/gettext/include .

mkdir lib
cp /usr/local/opt/gettext/lib/libintl.8.dylib lib/libintl.8.dylib
chmod 755 lib/libintl.8.dylib
