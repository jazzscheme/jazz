#! /bin/sh

GSTREAMER=/c/gstreamer/1.0/msvc_x86_64


mkd() {
    if [ -d $1 ]; then
      rm -r $1
    fi

    mkdir $1
}

mkd include
mkd lib


#
# lib
#

cpbundle() {
    cp $GSTREAMER/bin/$1.dll lib/$1.dll
}

cplink() {
    cp $GSTREAMER/lib/$1.a lib/$1.a
}

cpbundle pixman-1-0
cplink libpixman-1.dll


#
# include
#

cp $GSTREAMER/include/pixman-1/pixman.h include
cp $GSTREAMER/include/pixman-1/pixman-version.h include
