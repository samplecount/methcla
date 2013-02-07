# env
echo $CONFIGURATION # Debug | Release

ROOT_DIR="$1"

case $ACTION in
    clean)
        TARGET=clean ;;
    *)
        case $PLATFORM_NAME in
            iphoneos) TARGET=ios ;;
            iphonesimulator) TARGET=ios-simulator ;;
        esac ;;
esac

# CURRENT_ARCH
cd "$ROOT_DIR" && ./shake -j`sysctl -n hw.ncpu` $TARGET
