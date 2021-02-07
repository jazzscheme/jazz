#! /bin/sh

GSTREAMER=/c/gstreamer/1.0/msvc_x86_64
RNNOISE=/c/Home/gstreamer/rnnoise
WEBRTC=/c/Home/gstreamer/webrtc/x64/Release
WEBRTCAUDIOPROCESSING=/c/Home/gstreamer/webrtcaudioprocessing/build/plugin


mkd() {
    if [ -d $1 ]; then
      rm -r $1
    fi

    mkdir $1
}

mkd bin
mkd include
mkd lib
mkd libexec


#
# lib
#

cpbundle() {
    cp $GSTREAMER/lib/$1.dll lib/$1.dll
}

cpshared() {
    cp $GSTREAMER/bin/$1.dll bin/$1.dll
}

cplink() {
    cp $GSTREAMER/lib/$1.a lib/$1.a
}

mkdir lib/gstreamer-1.0
cpbundle gstreamer-1.0/gstalpha
cpbundle gstreamer-1.0/gstapp
cpbundle gstreamer-1.0/gstaudioconvert
cpbundle gstreamer-1.0/gstaudiofx
cpbundle gstreamer-1.0/gstaudiomixer
cpbundle gstreamer-1.0/gstaudioparsers
cpbundle gstreamer-1.0/gstaudiorate
cpbundle gstreamer-1.0/gstaudioresample
cpbundle gstreamer-1.0/gstaudiotestsrc
cpbundle gstreamer-1.0/gstautodetect
cpbundle gstreamer-1.0/gstavi
cpbundle gstreamer-1.0/gstcoreelements
cpbundle gstreamer-1.0/gstd3d
cpbundle gstreamer-1.0/gstdeinterlace
cpbundle gstreamer-1.0/gstdirectsound
cpbundle gstreamer-1.0/gstdirectsoundsrc
cpbundle gstreamer-1.0/gstid3demux
cpbundle gstreamer-1.0/gstisomp4
cpbundle gstreamer-1.0/gstjpeg
cpbundle gstreamer-1.0/gstlibav
cpbundle gstreamer-1.0/gstmatroska
cpbundle gstreamer-1.0/gstmediafoundation
cpbundle gstreamer-1.0/gstogg
cpbundle gstreamer-1.0/gstopus
cpbundle gstreamer-1.0/gstplayback
cpbundle gstreamer-1.0/gstsubparse
cpbundle gstreamer-1.0/gsttypefindfunctions
cpbundle gstreamer-1.0/gstvideoconvert
cpbundle gstreamer-1.0/gstvideofilter
cpbundle gstreamer-1.0/gstvideoparsersbad
cpbundle gstreamer-1.0/gstvideorate
cpbundle gstreamer-1.0/gstvideoscale
cpbundle gstreamer-1.0/gstvideotestsrc
cpbundle gstreamer-1.0/gstvolume
cpbundle gstreamer-1.0/gstvorbis
cpbundle gstreamer-1.0/gstvpx
cpbundle gstreamer-1.0/gstwasapi
cpbundle gstreamer-1.0/gstwavparse
cpbundle gstreamer-1.0/gstwinks
cpbundle gstreamer-1.0/gstwinscreencap
cpbundle gstreamer-1.0/gstx264
cpshared avcodec-58
cpshared avfilter-7
cpshared avformat-58
cpshared avutil-56
cpshared bz2
cpshared ffi-7
cpshared gio-2.0-0
cpshared glib-2.0-0
cpshared gmodule-2.0-0
cpshared gobject-2.0-0
cpshared graphene-1.0-0
cpshared gstallocators-1.0-0
cpshared gstapp-1.0-0
cpshared gstaudio-1.0-0
cpshared gstbadaudio-1.0-0
cpshared gstbase-1.0-0
cpshared gstcodecparsers-1.0-0
cpshared gstfft-1.0-0
cpshared gstgl-1.0-0
cpshared gstpbutils-1.0-0
cpshared gstreamer-1.0-0
cpshared gstriff-1.0-0
cpshared gstrtp-1.0-0
cpshared gsttag-1.0-0
cpshared gstvideo-1.0-0
cpshared intl-8
cpshared libjpeg-8
cpshared libogg-0
cpshared opus-0
cpshared orc-0.4-0
cpshared libpng16-16
cpshared libvorbis-0
cpshared libvorbisenc-2
cpshared libwinpthread-1
cpshared libx264-157
cpshared z-1
cpshared swresample-3
cplink libgstapp-1.0.dll
cplink libgstaudio-1.0.dll
cplink libgstbase-1.0.dll
cplink libgstpbutils-1.0.dll
cplink libgstreamer-1.0.dll
cplink libgobject-2.0.dll
cplink libglib-2.0.dll
cplink libintl.dll

cp $RNNOISE/rnnoise/windows/lib/rnnoise.dll bin/rnnoise.dll
cp $RNNOISE/build/plugin/gstrnnoise.dll lib/gstreamer-1.0/gstrnnoise.dll
cp $WEBRTC/webrtc.dll bin/webrtc.dll
cp $WEBRTCAUDIOPROCESSING/libgstwebrtcaudioprocessing.dll lib/gstreamer-1.0/libgstwebrtcaudioprocessing.dll


#
# libexec
#

mkdir libexec/gstreamer-1.0
cp $GSTREAMER/libexec/gstreamer-1.0/gst-plugin-scanner libexec/gstreamer-1.0


#
# include
#

cp -r $GSTREAMER/include/gstreamer-1.0 include
cp -r $GSTREAMER/include/glib-2.0 include
cp -r $GSTREAMER/lib/glib-2.0 lib
