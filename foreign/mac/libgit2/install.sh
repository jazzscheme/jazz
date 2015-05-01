#! /bin/sh

install_name_tool -id @rpath/libgit2.22.dylib lib/libgit2.22.dylib
install_name_tool -change /Users/cartier/Devel/system/libgit2/build/libgit2.22.dylib @rpath/libgit2.22.dylib lib/libgit2.22.dylib
