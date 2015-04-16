#! /bin/sh

install_name_tool -id @rpath/libfreetype.dylib lib/libfreetype.dylib
install_name_tool -change /usr/local/lib/libpng16.16.dylib @rpath/libpng15.15.dylib lib/libfreetype.dylib
