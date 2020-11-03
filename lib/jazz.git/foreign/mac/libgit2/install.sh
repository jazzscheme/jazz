#! /bin/sh

install_name_tool -id @rpath/libgit2.1.1.0.dylib lib/libgit2.1.1.0.dylib
install_name_tool -change /Users/cartier/Devel/system/libgit2-1.1.0/build/libgit2.1.1.0.dylib @rpath/libgit2.1.1.0.dylib lib/libgit2.1.1.0.dylib
