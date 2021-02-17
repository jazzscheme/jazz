#! /bin/sh

GSTREAMER=/Library/Frameworks/GStreamer.framework/Versions/1.0
# BUILD=/Users/cartier/Devel/gstreamer/gst-build/build/subprojects
RNNOISE=/Users/cartier/Devel/gstreamer/rnnoise
WEBRTC=/Users/cartier/Devel/gstreamer/webrtc/build
WEBRTCAUDIOPROCESSING=/Users/cartier/Devel/gstreamer/webrtcaudioprocessing/build/plugin

if [ -d gstreamer ]; then
  rm -r gstreamer
fi

mkdir gstreamer


#
# bin
#

cpbin() {
    cp $GSTREAMER/bin/$1 gstreamer/bin/$1
}

mkdir gstreamer/bin
cpbin gst-device-monitor-1.0
cpbin gst-discoverer-1.0
cpbin gst-inspect-1.0
cpbin gst-launch-1.0
cpbin gst-play-1.0
cpbin gst-typefind-1.0


#
# lib
#

cplib() {
    cp $GSTREAMER/lib/$1 gstreamer/lib/$1
}

cpbuild() {
    cp $BUILD/$1 gstreamer/lib/$2
}

mkdir gstreamer/lib
mkdir gstreamer/lib/gstreamer-1.0
cplib gstreamer-1.0/libgstalpha.dylib
cplib gstreamer-1.0/libgstapp.dylib
cplib gstreamer-1.0/libgstapplemedia.dylib
cplib gstreamer-1.0/libgstaudioconvert.dylib
cplib gstreamer-1.0/libgstaudiofx.dylib
cplib gstreamer-1.0/libgstaudiomixer.dylib
cplib gstreamer-1.0/libgstaudioparsers.dylib
cplib gstreamer-1.0/libgstaudiorate.dylib
cplib gstreamer-1.0/libgstaudioresample.dylib
cplib gstreamer-1.0/libgstaudiotestsrc.dylib
cplib gstreamer-1.0/libgstautodetect.dylib
cplib gstreamer-1.0/libgstavi.dylib
cplib gstreamer-1.0/libgstcoreelements.dylib
cplib gstreamer-1.0/libgstdeinterlace.dylib
cplib gstreamer-1.0/libgstid3demux.dylib
cplib gstreamer-1.0/libgstisomp4.dylib
cplib gstreamer-1.0/libgstjpeg.dylib
cplib gstreamer-1.0/libgstlibav.dylib
cplib gstreamer-1.0/libgstmatroska.dylib
cplib gstreamer-1.0/libgstmpegtsdemux.dylib
cplib gstreamer-1.0/libgstogg.dylib
cplib gstreamer-1.0/libgstopus.dylib
cplib gstreamer-1.0/libgstosxaudio.dylib
cplib gstreamer-1.0/libgstplayback.dylib
cplib gstreamer-1.0/libgstsubparse.dylib
cplib gstreamer-1.0/libgsttypefindfunctions.dylib
cplib gstreamer-1.0/libgstvideoconvert.dylib
cplib gstreamer-1.0/libgstvideofilter.dylib
cplib gstreamer-1.0/libgstvideoparsersbad.dylib
cplib gstreamer-1.0/libgstvideorate.dylib
cplib gstreamer-1.0/libgstvideoscale.dylib
cplib gstreamer-1.0/libgstvideotestsrc.dylib
cplib gstreamer-1.0/libgstvolume.dylib
cplib gstreamer-1.0/libgstvorbis.dylib
cplib gstreamer-1.0/libgstwavparse.dylib
# cplib gstreamer-1.0/libgstwebrtcdsp.dylib
# cpbuild gst-plugins-bad/ext/webrtcdsp/libgstwebrtcdsp.dylib gstreamer-1.0/libgstwebrtcdsp.dylib
cplib gstreamer-1.0/libgstx264.dylib
cplib libavcodec.58.dylib
cplib libavfilter.7.dylib
cplib libavformat.58.dylib
cplib libavutil.56.dylib
cplib libbz2.1.dylib
cplib libffi.7.dylib
cplib libgio-2.0.0.dylib
cplib libglib-2.0.0.dylib
cplib libgmodule-2.0.0.dylib
cplib libgobject-2.0.0.dylib
cplib libgraphene-1.0.0.dylib
cplib libgstallocators-1.0.0.dylib
cplib libgstapp-1.0.0.dylib
cplib libgstaudio-1.0.0.dylib
cplib libgstbadaudio-1.0.0.dylib
cplib libgstbase-1.0.0.dylib
cplib libgstcodecparsers-1.0.0.dylib
cplib libgstfft-1.0.0.dylib
cplib libgstgl-1.0.0.dylib
cplib libgstmpegts-1.0.0.dylib
cplib libgstpbutils-1.0.0.dylib
cplib libgstreamer-1.0.0.dylib
cplib libgstriff-1.0.0.dylib
cplib libgstrtp-1.0.0.dylib
cplib libgsttag-1.0.0.dylib
cplib libgstvideo-1.0.0.dylib
cplib libgstvulkan-1.0.0.dylib
cplib libintl.8.dylib
cplib libjpeg.8.dylib
cplib libMoltenVK.dylib
cplib libogg.0.dylib
cplib libopus.0.dylib
cplib liborc-0.4.0.dylib
cplib libpng16.16.dylib
cplib libswresample.3.dylib
cplib libvorbis.0.dylib
cplib libvorbisenc.2.dylib
# cplib libwebrtc_audio_processing.0.dylib
cplib libx264.157.dylib
cplib libz.1.dylib

cp $RNNOISE/rnnoise/lib/librnnoise.0.dylib gstreamer/lib
cp $RNNOISE/build/plugin/libgstrnnoise.dylib gstreamer/lib/gstreamer-1.0/libgstrnnoise.dylib
cp $WEBRTC/libwebrtc.dylib gstreamer/lib
cp $WEBRTCAUDIOPROCESSING/libgstwebrtcaudioprocessing.dylib gstreamer/lib/gstreamer-1.0/libgstwebrtcaudioprocessing.dylib
install_name_tool -change @rpath/libwebrtc.dylib @rpath/gstreamer/lib/libwebrtc.dylib gstreamer/lib/gstreamer-1.0/libgstwebrtcaudioprocessing.dylib


#
# libexec
#

mkdir gstreamer/libexec
mkdir gstreamer/libexec/gstreamer-1.0
cp $GSTREAMER/libexec/gstreamer-1.0/gst-plugin-scanner gstreamer/libexec/gstreamer-1.0


#
# include
#

mkdir gstreamer/include
cp -r $GSTREAMER/include/gstreamer-1.0 gstreamer/include
cp -r $GSTREAMER/include/glib-2.0 gstreamer/include
cp -r $GSTREAMER/lib/glib-2.0 gstreamer/lib
