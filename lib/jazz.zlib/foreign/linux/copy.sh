#! /bin/sh

GSTREAMER=~/Devel/gstreamer/cerbero/build/dist/linux_x86_64


#
# lib
#

if [ -d lib ]; then
  rm -r lib
fi

mkdir lib

cpshared() {
    cp $GSTREAMER/lib/$1.$2 lib/$1.$2
    patchelf --set-rpath '$ORIGIN' lib/$1.$2
    ln -sr lib/$1.$2 lib/$1
}

cpshared libz.so 1


#
# include
#

if [ -d include ]; then
  rm -r include
fi

mkdir include
cp $GSTREAMER/include/zconf.h include
cp $GSTREAMER/include/zlib.h include
