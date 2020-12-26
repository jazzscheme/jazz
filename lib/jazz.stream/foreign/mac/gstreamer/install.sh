#! /bin/sh

JAZZ=../../../../../../cache/develop/jazz

$JAZZ -run relocate directory gstreamer /Library/Frameworks/GStreamer.framework/Versions/1.0 @rpath/gstreamer
