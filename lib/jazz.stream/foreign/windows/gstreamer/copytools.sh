#! /bin/sh

GSTREAMER=/c/gstreamer/1.0/msvc_x86_64


#
# bin
#

cpbin() {
    cp $GSTREAMER/bin/$1.exe bin/$1.exe
}

cpbin gst-device-monitor-1.0
cpbin gst-discoverer-1.0
cpbin gst-inspect-1.0
cpbin gst-launch-1.0
cpbin gst-play-1.0
cpbin gst-typefind-1.0
