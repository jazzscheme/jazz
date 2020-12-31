#! /bin/sh

GSTREAMERDISTR=/Users/cartier/Devel/gstreamer/gst-build/distr

JAZZ=../../../../../../cache/develop/jazz


cpdistr() {
    cp $GSTREAMERDISTR/lib/$1 gstreamer/lib/$2
}

relocate() {
    echo relocating $1...
    $JAZZ -run relocate change-name gstreamer lib/$1 /Users/cartier/Devel/gstreamer/gst-build/distr @rpath/gstreamer
    $JAZZ -run relocate change-dependencies gstreamer lib/$1 /Users/cartier/Devel/gstreamer/gst-build/distr @rpath
    $JAZZ -run relocate change-dependencies gstreamer lib/$1 /Library/Frameworks/GStreamer.framework/Versions/1.0 @rpath
    $JAZZ -run relocate add-rpath gstreamer lib/$1 @loader_path/..
}

patch() {
  cpdistr $1 $1
  relocate $1
}

patch libgstreamer-1.0.0.dylib
patch libgstbase-1.0.0.dylib
patch libgstaudio-1.0.0.dylib
patch liborc-0.4.0.dylib
