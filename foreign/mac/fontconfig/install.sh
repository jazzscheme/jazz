#! /bin/sh

install_name_tool -id @rpath/libfontconfig.1.dylib lib/libfontconfig.1.dylib
install_name_tool -change /usr/local/lib/libfreetype.6.dylib @rpath/libfreetype.6.dylib lib/libfontconfig.1.dylib
