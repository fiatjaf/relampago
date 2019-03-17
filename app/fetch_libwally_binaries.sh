#!/usr/bin/env bash
# Downloads and installs the pre-built wally libraries for use by GreenBits
set -e

# The version of wally to fetch and its sha256 checksum for integrity checking
TARBALL="wallycore-android-jni.tar.gz"
URL="https://github.com/ElementsProject/libwally-core/releases/download/release_0.6.3/${TARBALL}"
SHA256="2979b7cba8d895e0c0c927fee17a58a7da8fde133a3f0c25f365a8d6559a16d8"

command -v curl >/dev/null 2>&1 || { echo >&2 "$1 not found, exiting."; exit 1; }
command -v gzip >/dev/null 2>&1 || { echo >&2 "$1 not found, exiting."; exit 1; }
command -v shasum >/dev/null 2>&1 || { echo >&2 "$1 not found, exiting."; exit 1; }
command -v javac >/dev/null 2>&1 || { echo >&2 "$1 not found, exiting."; exit 1; }
command -v jar >/dev/null 2>&1 || { echo >&2 "$1 not found, exiting."; exit 1; }

# Find out where we are being run from to get paths right
OLD_PWD=$(pwd)
APP_ROOT=${OLD_PWD}
if [ -d "${APP_ROOT}/app" ]; then
    APP_ROOT="${APP_ROOT}/app"
fi
WALLY_JAVA_DIR="${APP_ROOT}/libwally-core/src/swig_java/src/com/blockstream/libwally"

# Clean up any previous install
rm -rf wallycore-android-jni* ${APP_ROOT}/src/main/jniLibs ${WALLY_JAVA_DIR}

# Fetch, validate and decompress wally
curl -sL -o ${TARBALL} "${URL}"
echo "${SHA256}  ${TARBALL}" | shasum -a 256 --check
gzip -d wallycore-android-jni.tar.gz && tar xf wallycore-android-jni.tar

# Move the libraries and Java wrapper where we need them
mv wallycore-android-jni/lib/ ${APP_ROOT}/src/main/jniLibs
mkdir -p ${WALLY_JAVA_DIR}
mv wallycore-android-jni/src/swig_java/src/com/blockstream/libwally/Wally.java ${WALLY_JAVA_DIR}

# Create the wally jar file for building against
cd ${APP_ROOT}/libwally-core/src/swig_java/src
javac -source 1.7 -target 1.7 com/blockstream/libwally/Wally.java
jar cf ../wallycore.jar com/blockstream/libwally/Wally*class

# Cleanup
rm -r ${OLD_PWD}/wallycore-android-jni* ${WALLY_JAVA_DIR}
