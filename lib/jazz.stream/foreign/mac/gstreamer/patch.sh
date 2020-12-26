#! /bin/sh

GSTREAMERDISTR=/Users/cartier/Devel/gstreamer/gst-build/distr

JAZZ=../../../../../../cache/develop/jazz


#
# lib
#

cpdistr() {
    cp $GSTREAMERDISTR/lib/$1 gstreamer/lib/$2
}

cpdistr libgstreamer-1.0.0.dylib libgstreamer-1.0.0.dylib
cpdistr libgstaudio-1.0.0.dylib libgstaudio-1.0.0.dylib
cpdistr liborc-0.4.0.dylib liborc-0.4.0.dylib

relocate() {
    echo relocating $1...
    $JAZZ -run relocate change-name gstreamer lib/$1 /Users/cartier/Devel/gstreamer/gst-build/distr @rpath/gstreamer
    $JAZZ -run relocate change-dependencies gstreamer lib/$1 /Users/cartier/Devel/gstreamer/gst-build/distr @rpath
    $JAZZ -run relocate change-dependencies gstreamer lib/$1 /Library/Frameworks/GStreamer.framework/Versions/1.0 @rpath
    $JAZZ -run relocate add-rpath gstreamer lib/$1 @loader_path/..
}

relocate libgstreamer-1.0.0.dylib
relocate libgstaudio-1.0.0.dylib
relocate liborc-0.4.0.dylib
