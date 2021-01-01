#! /bin/sh

GSTREAMERBUILD=/c/Home/gstreamer/gst-build/build


cpshared() {
    cp $GSTREAMERBUILD/$1 bin/$2
}

patch() {
  cpshared $1 $2
}

patch subprojects/gstreamer/gst/libgstreamer-1.0-0.dll libgstreamer-1.0-0.dll
patch subprojects/gstreamer/libs/gst/base/libgstbase-1.0-0.dll libgstbase-1.0-0.dll
patch subprojects/gst-plugins-base/gst-libs/gst/audio/libgstaudio-1.0-0.dll libgstaudio-1.0-0.dll
patch subprojects/orc/orc/liborc-0.4-0.dll liborc-0.4-0.dll
