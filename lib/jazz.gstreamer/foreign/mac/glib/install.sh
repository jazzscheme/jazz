#! /bin/sh

install_name_tool -id @rpath/libglib-2.0.0.dylib lib/libglib-2.0.0.dylib
install_name_tool -change /usr/local/opt/gettext/lib/libintl.8.dylib @rpath/libintl.8.dylib lib/libglib-2.0.0.dylib

install_name_tool -id @rpath/libgobject-2.0.0.dylib lib/libgobject-2.0.0.dylib
install_name_tool -change /usr/local/Cellar/glib/2.52.3/lib/libglib-2.0.0.dylib @rpath/libglib-2.0.0.dylib lib/libgobject-2.0.0.dylib
install_name_tool -change /usr/local/opt/gettext/lib/libintl.8.dylib @rpath/libintl.8.dylib lib/libgobject-2.0.0.dylib

install_name_tool -id @rpath/libgmodule-2.0.0.dylib lib/libgmodule-2.0.0.dylib
install_name_tool -change /usr/local/Cellar/glib/2.52.3/lib/libglib-2.0.0.dylib @rpath/libglib-2.0.0.dylib lib/libgmodule-2.0.0.dylib
install_name_tool -change /usr/local/opt/gettext/lib/libintl.8.dylib @rpath/libintl.8.dylib lib/libgmodule-2.0.0.dylib
