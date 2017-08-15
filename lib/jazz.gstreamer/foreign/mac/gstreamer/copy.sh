#! /bin/sh

if [ -d include ]; then
  rm -r include
fi

if [ -d lib ]; then
  rm -r lib
fi

mkdir include
cp -r /usr/local/Cellar/gstreamer/1.12.2/include/gstreamer-1.0 include/gstreamer-1.0

mkdir lib
cp /usr/local/Cellar/gstreamer/1.12.2/lib/libgstreamer-1.0.0.dylib lib/libgstreamer-1.0.0.dylib
chmod 755 lib/libgstreamer-1.0.0.dylib
