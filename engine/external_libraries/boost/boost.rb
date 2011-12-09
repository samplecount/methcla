#!/usr/bin/env ruby

#===============================================================================
# Filename:  boost.sh
# Author:    Pete Goodliffe
# Copyright: (c) Copyright 2009 Pete Goodliffe
# Licence:   Please feel free to use this, with attribution
#===============================================================================
#
# Builds a Boost framework for the iPhone.
# Creates a set of universal libraries that can be used on an iPhone and in the
# iPhone simulator. Then creates a pseudo-framework to make using boost in Xcode
# less painful.
#
# To configure the script, define:
#    BOOST_LIBS:        which libraries to build
#    BOOST_VERSION:     version number of the boost library (e.g. 1_41_0)
#    IPHONE_SDKVERSION: iPhone SDK version (e.g. 3.0)
#
# Then go get the source tar.bz of the boost you want to build, shove it in the
# same directory as this script, and run "./boost.sh". Grab a cuppa. And voila.
#===============================================================================

require 'fileutils'

include FileUtils

class BoostLib
  attr_reader :name
  def initialize(name, libs=nil)
    @name = name
    @libs = libs || [name]
  end
  def lib_names
    @libs.collect { |l| "libboost_#{l}.a" }
  end
  def libs_arm
    lib_names.collect { |la|
      "#{BOOST_SRC}/bin.v2/libs/#{name}/build/darwin-4.2.1~iphone/release/architecture-arm/link-static/macosx-version-iphone-#{IPHONE_SDKVERSION}/target-os-iphone/threading-multi/#{la}"
    }
  end
  def libs_i386
    lib_names.collect { |la|
      "#{BOOST_SRC}/bin.v2/libs/#{lib.name}/build/darwin-4.2.1~iphonesim/release/architecture-x86/link-static/macosx-version-iphonesim-#{IPHONE_SDKVERSION}/target-os-iphone/threading-multi/#{la}"
    }
  end
end

def boost_lib(name, libs=nil)
  BoostLib.new(name, libs)
end

BOOST_VERSION = '1_48_0'
BOOST_LIBS = [
    boost_lib("thread") \
  , boost_lib("signals") \
  , boost_lib("filesystem") \
  , boost_lib("regex") \
  , boost_lib("program_options") \
  , boost_lib("system") \
  , boost_lib("test", %w(prg_exec_monitor test_exec_monitor unit_test_framework)) ]
IPHONE_SDKVERSION = 4.2
EXTRA_CPPFLAGS = "" # "-DBOOST_AC_USE_PTHREADS -DBOOST_SP_USE_PTHREADS"

# The EXTRA_CPPFLAGS definition works around a thread race issue in
# shared_ptr. I encountered this historically and have not verified that
# the fix is no longer required. Without using the posix thread primitives
# an invalid compare-and-swap ARM instruction (non-thread-safe) was used for the
# shared_ptr use count causing nasty and subtle bugs.
#
# Should perhaps also consider/use instead: -BOOST_SP_USE_PTHREADS

TARBALLDIR = Dir.pwd
SRCDIR = Dir.pwd
# BUILDDIR = File.join(Dir.pwd, 'build')
PREFIXDIR = Dir.pwd
# FRAMEWORKDIR = Dir.pwd

BOOST_TARBALL    = File.join(TARBALLDIR, "boost_#{BOOST_VERSION}.tar.bz2")
BOOST_SRC        = File.join(SRCDIR, "boost_#{BOOST_VERSION}")
BJAM_USER_CONFIG = File.join(BOOST_SRC, 'tools/build/v2/user-config.jam')

#===============================================================================

ARM_DEV_DIR = '/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/'
SIM_DEV_DIR = '/Developer/Platforms/iPhoneSimulator.platform/Developer/usr/bin/'

#===============================================================================

puts "BOOST_VERSION:     #{BOOST_VERSION}"
puts "BOOST_LIBS:        #{BOOST_LIBS.collect { |l| l.name }.join(' ')}"
puts "BOOST_TARBALL:     #{BOOST_TARBALL}"
puts "BOOST_SRC:         #{BOOST_SRC}"
# puts "BUILDDIR:          #{BUILDDIR}"
puts "PREFIXDIR:         #{PREFIXDIR}"
# puts "FRAMEWORKDIR:      #{FRAMEWORKDIR}"
puts "IPHONE_SDKVERSION: #{IPHONE_SDKVERSION}"
puts ""

#===============================================================================
# Functions
#===============================================================================

def abort(msg)
  puts
  puts "Aborted: #{msg}"
  exit(1)
end

def doneSection
  puts "================================================================="
  puts "Done"
  puts
end

def wrap(msg, &body)
  puts "================================================================="
  puts msg
  body.call
  doneSection
end

#===============================================================================

def clean
  rm_rf BOOST_SRC
end

#===============================================================================

def cleanAll
  clean
  rm_rf File.join(PREFIXDIR, 'include')
  rm_rf File.join(PREFIXDIR, 'lib')
  begin
    rmdir PREFIXDIR
  rescue
  end
end

#===============================================================================
def unpack
  if File.directory?(BOOST_SRC)
    puts "    Unpacking #{BJAM_USER_CONFIG} ..."
    system "tar xjf #{BOOST_TARBALL} #{BJAM_USER_CONFIG}"
  else
    puts "    Unpacking to #{BOOST_SRC} ..."
    system "tar xfj #{BOOST_TARBALL}"
  end
end

#===============================================================================

def copyMissingHeaders
  # These files are missing in the ARM iPhoneOS SDK, but they are in the simulator.
  # They are supported on the device, so we copy them from x86 SDK to a staging area
  # to use them on ARM, too.
  for header in %w(crt_externs bzlib)
    cp("/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator#{IPHONE_SDKVERSION}.sdk/usr/include/#{header}.h", BOOST_SRC)
  end
end

#===============================================================================

def writeBjamUserConfig
  # You need to do this to point bjam at the right compiler
  File.open(BJAM_USER_CONFIG, 'a') { |io|
    io << <<EOF
using darwin : 4.2.1~iphone
 : /Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/gcc-4.2 -arch armv7 -mthumb -fvisibility=hidden -fvisibility-inlines-hidden #{EXTRA_CPPFLAGS}
 : <striper>
 : <architecture>arm <target-os>iphone
 ;
using darwin : 4.2.1~iphonesim
 : /Developer/Platforms/iPhoneSimulator.platform/Developer/usr/bin/gcc-4.2 -arch i386 -fvisibility=hidden -fvisibility-inlines-hidden #{EXTRA_CPPFLAGS}
 : <striper>
 : <architecture>x86 <target-os>iphone
 ;
EOF
  }
end

#===============================================================================

def bootstrap
  libs = BOOST_LIBS.collect { |l| l.name}.join(',')
  system "cd #{BOOST_SRC} && ./bootstrap.sh --with-libraries=#{libs}"
end

#===============================================================================

def build
  system "cd #{BOOST_SRC} && ./bjam --prefix=\"#{PREFIXDIR}\" toolset=darwin architecture=arm target-os=iphone macosx-version=iphone-#{IPHONE_SDKVERSION} define=_LITTLE_ENDIAN link=static install"
  system "cd #{BOOST_SRC} && ./bjam toolset=darwin architecture=x86 target-os=iphone macosx-version=iphonesim-#{IPHONE_SDKVERSION} link=static stage"
end

#===============================================================================

# $1: Name of a boost library to lipoficate (technical term)
def lipoficate(lib)
  puts "liboficate: #{lib.name}"
  
  libdir = File.join(PREFIXDIR, 'lib')
  mkdir_p(libdir)

  lib.lib_names.each { |la|
    arm = "#{BOOST_SRC}/bin.v2/libs/#{lib.name}/build/darwin-4.2.1~iphone/release/architecture-arm/link-static/macosx-version-iphone-#{IPHONE_SDKVERSION}/target-os-iphone/threading-multi/#{la}"
    x86 = "#{BOOST_SRC}/bin.v2/libs/#{lib.name}/build/darwin-4.2.1~iphonesim/release/architecture-x86/link-static/macosx-version-iphonesim-#{IPHONE_SDKVERSION}/target-os-iphone/threading-multi/#{la}"
    unless system "lipo -create \"#{arm}\" \"#{x86}\" -o \"#{libdir}/#{la}\""
      abort "Lipo #{la} failed"
    end
  }
end

# This creates universal versions of each individual boost library
def lipoAllLibraries
  BOOST_LIBS.each { |l| lipoficate(l) }
end

# def scrunchAllLibsTogetherInOneLibPerPlatform
#     ARM_ARCHS = ['armv6', 'armv7']
#     SIM_ARCHS = ['i386']
#     ARCHS = ARM_ARCHS + SIM_ARCHS
# 
#     # libs_arm = BOOST_LIBS.collect { |l| l.libs_arm }.flatten
#     # libs_sim = BOOST_LIBS.collect { |l| l.libs_i386 }.flatten
# 
#     # for NAME in $BOOST_LIBS; do
#     #     ALL_LIBS_ARM="$ALL_LIBS_ARM $BOOST_SRC/bin.v2/libs/$NAME/build/darwin-4.2.1~iphone/release/architecture-arm/link-static/macosx-version-iphone-$IPHONE_SDKVERSION/target-os-iphone/threading-multi/libboost_$NAME.a";
#     #     ALL_LIBS_SIM="$ALL_LIBS_SIM $BOOST_SRC/bin.v2/libs/$NAME/build/darwin-4.2.1~iphonesim/release/architecture-x86/link-static/macosx-version-iphonesim-$IPHONE_SDKVERSION/target-os-iphone/threading-multi/libboost_$NAME.a";
#     # done;
# 
#     mkdir_p(File.join(BUILDDIR, "armv6/obj")
#     mkdir_p(File.join(BUILDDIR, "armv7/obj")
#     mkdir_p(File.join(BUILDDIR, "i386/obj")
# 
#     puts "Splitting all existing fat binaries..."
#     BOOST_LIBS.each { |lib|
#         # ALL_LIBS="$ALL_LIBS libboost_$NAME.a"
#         lib.libs_arm.each { |la|
#           system "lipo \"#{la}\" -thin armv6 -o \"#{BUILDDIR}/armv6/#{File.basename(la)}\""
#           system "lipo \"#{la}\" -thin armv7 -o \"#{BUILDDIR}/armv7/#{File.basename(la)}\""
#           # lipo "$BOOST_SRC/bin.v2/libs/$NAME/build/darwin-4.2.1~iphone/release/architecture-arm/link-static/macosx-version-iphone-$IPHONE_SDKVERSION/target-os-iphone/threading-multi/libboost_$NAME.a" -thin armv7 -o $BUILDDIR/armv7/libboost_$NAME.a
#         }
#         lib.libs_i386.each { |la|
#           cp(ls, File.join(BUILDDIR, "i386"))
#         }
#     done
# 
#     puts "Decomposing each architecture's .a files"
#     BOOST_LIBS.each { |lib|
#         puts "Decomposing #{lib.name}..."
#         ARCHS.each { |arch|
#           system "(cd #{BUILDDIR}/#{arch}/obj; ar -x ../#{lib.name} )"
#         }
#     }
# 
#     puts "Linking each architecture into an uberlib ($ALL_LIBS => libboost.a )"
#     ARCHS.each { |arch| rm_f(File.join(BUILDDIR, arch, "libboost.a")) }
#     ARM_ARCHS.each { |arch|
#       puts "...#{arch}"
#       system "(cd #{BUILDDIR}/#{arch}; #{ARM_DEV_DIR}/ar crus libboost.a obj/*.o; )"
#     }
#     SIM_ARCHS.each { |arch|
#       puts "...#{arch}"
#       system "(cd #{BUILDDIR}/#{arch}; #{SIM_DEV_DIR}/ar crus libboost.a obj/*.o; )"
#     }
# }

# def buildUniversalBinary
#   system "lipo \
#           -create \
#           -arch armv6 \"#{BUILDDIR}/armv6/libboost.a\" \
#           -arch armv7 \"#{BUILDDIR}/armv7/libboost.a\" \
#           -arch i386  \"#{BUILDDIR}/i386/libboost.a\" \
#           -o          \"#{BUILDDIR}\""
#     || abort "Lipo failed"
# end

#===============================================================================

#                     VERSION_TYPE='Alpha'
#                   FRAMEWORK_NAME='boost'
#                FRAMEWORK_VERSION='A'
# 
#        FRAMEWORK_CURRENT_VERSION=BOOST_VERSION
#  FRAMEWORK_COMPATIBILITY_VERSION=BOOST_VERSION
# 
# def buildFramework
#     FRAMEWORK_BUNDLE = File.join(FRAMEWORKDIR, FRAMEWORK_NAME + ".framework")
# 
#     rm_rf(FRAMEWORK_BUNDLE)
# 
#     puts "Framework: Setting up directories..."
#     mkdir_p(FRAMEWORK_BUNDLE)
#     mkdir_p(File.join(FRAMEWORK_BUNDLE, 'Versions') 
#     mkdir_p(File.join(FRAMEWORK_BUNDLE, 'Versions', FRAMEWORK_VERSION)
#     mkdir_p(File.join(FRAMEWORK_BUNDLE, 'Versions', FRAMEWORK_VERSION, 'Resources')
#     mkdir_p(File.join(FRAMEWORK_BUNDLE, 'Versions', FRAMEWORK_VERSION, 'Headers')
#     mkdir_p(File.join(FRAMEWORK_BUNDLE, 'Versions', FRAMEWORK_VERSION, 'Documentation')
# 
#     puts "Framework: Creating symlinks..."
#     ln_s(FRAMEWORK_VERSION                             , File.join(FRAMEWORK_BUNDLE, 'Versions/Current'))
#     ln_s('Versions/Current/Headers'                    , File.join(FRAMEWORK_BUNDLE, 'Headers'))
#     ln_s('Versions/Current/Resources'                  , File.join(FRAMEWORK_BUNDLE, 'Resources'))
#     ln_s('Versions/Current/Documentation'              , File.join(FRAMEWORK_BUNDLE, 'Documentation'))
#     ln_s(File.join('Versions/Current', FRAMEWORK_NAME) , File.join(FRAMEWORK_BUNDLE, FRAMEWORK_NAME))
# 
#     FRAMEWORK_INSTALL_NAME = File.join(FRAMEWORK_BUNDLE, 'Versions', FRAMEWORK_VERSION, FRAMEWORK_NAME)
# 
#     puts "Lipoing library into #{FRAMEWORK_INSTALL_NAME}..."
#     system "lipo \
#             -create \
#             -arch armv6 \"#{BUILDDIR}/armv6/libboost.a\" \
#             -arch armv7 \"#{BUILDDIR}/armv7/libboost.a\" \
#             -arch i386  \"#{BUILDDIR}/i386/libboost.a\" \
#             -o          \"#{FRAMEWORK_INSTALL_NAME}\""
#       || abort "Lipo failed"
# 
#     puts "Framework: Copying includes..."
#     cp_r File.join(PREFIXDIR, 'include/boost/'), File.join(FRAMEWORK_BUNDLE, 'Headers')
# 
#     echo "Framework: Creating plist..."
#     File.open(File.join(FRAMEWORK_BUNDLE, 'Resources/Info.plist'), 'w') { |io|
#       io << <<EOF
# <?xml version="1.0" encoding="UTF-8"?>
# <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
# <plist version="1.0">
# <dict>
#     <key>CFBundleDevelopmentRegion</key>
#     <string>English</string>
#     <key>CFBundleExecutable</key>
#     <string>#{FRAMEWORK_NAME}</string>
#     <key>CFBundleIdentifier</key>
#     <string>org.boost</string>
#     <key>CFBundleInfoDictionaryVersion</key>
#     <string>6.0</string>
#     <key>CFBundlePackageType</key>
#     <string>FMWK</string>
#     <key>CFBundleSignature</key>
#     <string>????</string>
#     <key>CFBundleVersion</key>
#     <string>#{FRAMEWORK_CURRENT_VERSION}</string>
# </dict>
# </plist>
# EOF
#     doneSection
# end

#===============================================================================
# Execution starts here
#===============================================================================

cmd = ARGV[0] || 'build'

case cmd
  when 'build'
    unless File.file?(BOOST_TARBALL)
      abort "Source tarball missing."
    end
    wrap("Unpacking source tarball") { unpack }
    wrap("Copying missing headers") { copyMissingHeaders }
    wrap("Creating bjam user config") { writeBjamUserConfig }
    wrap("Bootstrapping build system") { bootstrap }
    wrap("Building libraries") { build }
    wrap("Creating universal binary libraries") { lipoAllLibraries }
    # scrunchAllLibsTogetherInOneLibPerPlatform
    # buildFramework
  when 'clean'
    wrap("Cleaning build tree") { clean }
  when 'clean-all'
    wrap("Cleaning everything") { cleanAll }
  else
    puts "Unknown command #{cmd}"
    exit(1)
end

puts "Completed successfully"

#===============================================================================

