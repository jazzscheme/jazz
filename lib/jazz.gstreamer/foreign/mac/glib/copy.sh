#! /bin/sh

if [ -d include ]; then
  rm -r include
fi

if [ -d lib ]; then
  rm -r lib
fi

mkdir include
cp -r /usr/local/Cellar/glib/2.52.3/include/glib-2.0 include/glib-2.0
cp -r /usr/local/Cellar/glib/2.52.3/lib/glib-2.0/include .

mkdir lib
cp /usr/local/Cellar/glib/2.52.3/lib/libglib-2.0.0.dylib lib/libglib-2.0.0.dylib
cp /usr/local/Cellar/glib/2.52.3/lib/libgobject-2.0.0.dylib lib/libgobject-2.0.0.dylib
cp /usr/local/Cellar/glib/2.52.3/lib/libgmodule-2.0.0.dylib lib/libgmodule-2.0.0.dylib
chmod 755 lib/libglib-2.0.0.dylib
chmod 755 lib/libgobject-2.0.0.dylib
chmod 755 lib/libgmodule-2.0.0.dylib
