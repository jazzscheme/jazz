#! /bin/sh

install_name_tool -id @rpath/libcairo.2.dylib lib/libcairo.2.dylib
install_name_tool -change /opt/X11/lib/libpixman-1.0.dylib @rpath/libpixman-1.0.dylib lib/libcairo.2.dylib
install_name_tool -change /opt/X11/lib/libfontconfig.1.dylib @rpath/libfontconfig.dylib lib/libcairo.2.dylib
install_name_tool -change /opt/X11/lib/libfreetype.6.dylib @rpath/libfreetype.dylib lib/libcairo.2.dylib
install_name_tool -change /opt/X11/lib/libpng15.15.dylib @rpath/libpng15.15.dylib lib/libcairo.2.dylib
