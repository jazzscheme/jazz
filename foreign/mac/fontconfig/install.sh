#! /bin/sh

install_name_tool -id @rpath/libfontconfig.dylib lib/libfontconfig.dylib
install_name_tool -change /usr/local/lib/libfreetype.6.dylib @rpath/libfreetype.dylib lib/libfontconfig.dylib
