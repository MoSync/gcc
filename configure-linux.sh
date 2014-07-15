#!/bin/sh

#---------------------------------------------------------------------------------
# Source and Install directories
#---------------------------------------------------------------------------------

SRCDIR=$PWD/gcc-3.4.6                        # the sourcecode dir for gcc
                                             # This must be specified in the format shown here
                                             # as one of the tools built during the process will fail
                                             # if absolute paths are specified
                                             # the example here assumes that the gcc source directory
                                             # is at the same level as the script

prefix=/usr/mosync                           # installation directory
                                             # This must be specified in the format shown here
                                             # or gcc won't be able to find it's libraries and includes
                                             # if you move the installation

builddir=./build/gcc

#---------------------------------------------------------------------------------
# set the path for the installed binutils
#---------------------------------------------------------------------------------

#export PATH=${PATH}:/c/cross-gcc/mosync/bin

#---------------------------------------------------------------------------------
# set the target and compiler flags
#---------------------------------------------------------------------------------

target=mapip
progpref=mapip-

export CFLAGS='-O2 -pipe -m32'
export CXXFLAGS='-O2 -pipe -m32'
export LDFLAGS='-s -m32'
export DEBUG_FLAGS=''

#---------------------------------------------------------------------------------
# build and install just the c compiler
#---------------------------------------------------------------------------------

mkdir -p $builddir
cd $builddir

$SRCDIR/configure \
        --enable-languages=c,c++ \
        --with-gcc --with-stabs \
        --disable-shared --disable-threads --disable-win32-registry --disable-nls \
	--prefix=$prefix \
        --target=$target \
        --without-headers \
        --program-prefix=$progpref -v \
	--build=i686-linux-gnu \
	--host=i686-linux-gnu \
        2>&1 | tee gcc_configure.log
