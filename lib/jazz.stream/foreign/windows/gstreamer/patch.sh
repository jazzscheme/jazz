#! /bin/sh

GSTREAMERBUILD=/c/Home/gstreamer/gst-build/build


cpshared() {
    cp $GSTREAMERBUILD/$1 bin/$2
}

patch() {
  cpshared $1 $2
}

patch subprojects/gstreamer/gst/gstreamer-1.0-0.dll gstreamer-1.0-0.dll
patch subprojects/gstreamer/libs/gst/base/gstbase-1.0-0.dll gstbase-1.0-0.dll
patch subprojects/gst-plugins-base/gst-libs/gst/audio/gstaudio-1.0-0.dll gstaudio-1.0-0.dll
patch subprojects/orc/orc/orc-0.4-0.dll orc-0.4-0.dll
