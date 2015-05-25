#! /bin/sh

install_name_tool -id @rpath/libfreetype.6.dylib lib/libfreetype.6.dylib
install_name_tool -change /Users/cartier/Devel/local/lib/libpng16.16.dylib @rpath/libpng16.16.dylib lib/libfreetype.6.dylib
