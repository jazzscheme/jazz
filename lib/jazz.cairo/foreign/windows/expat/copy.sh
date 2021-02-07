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

cpbundle libexpat-1
cplink libexpat.dll


#
# include
#

cp $GSTREAMER/include/expat.h include
cp $GSTREAMER/include/expat_external.h include
