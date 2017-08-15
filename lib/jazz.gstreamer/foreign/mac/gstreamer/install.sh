#! /bin/sh

install_name_tool -id @rpath/libgstreamer-1.0.0.dylib lib/libgstreamer-1.0.0.dylib
install_name_tool -change /usr/local/opt/glib/lib/libgobject-2.0.0.dylib @rpath/libgobject-2.0.0.dylib lib/libgstreamer-1.0.0.dylib
install_name_tool -change /usr/local/opt/glib/lib/libgmodule-2.0.0.dylib @rpath/libgmodule-2.0.0.dylib lib/libgstreamer-1.0.0.dylib
install_name_tool -change /usr/local/opt/glib/lib/libglib-2.0.0.dylib @rpath/libglib-2.0.0.dylib lib/libgstreamer-1.0.0.dylib
install_name_tool -change /usr/local/opt/gettext/lib/libintl.8.dylib @rpath/libintl.8.dylib lib/libgstreamer-1.0.0.dylib
