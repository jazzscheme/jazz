#! /bin/sh

# install_name_tool -id @executable_path/libcairo.2.dylib lib/libcairo.2.dylib

install_name_tool -id @rpath/libcairo.2.dylib lib/libcairo.2.dylib
