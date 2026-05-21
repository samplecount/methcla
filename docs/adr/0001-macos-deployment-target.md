# macOS deployment target set to 16

The CMake build sets `CMAKE_OSX_DEPLOYMENT_TARGET=26` in all presets. macOS 26 (Tahoe, the 2025 release where Apple synced macOS versioning with iOS) is the minimum supported version; older releases are not tested and not guaranteed to link correctly due to symbol availability differences. This is a deliberate trade-off: we accept dropping older hardware in exchange for being able to use modern system APIs (including ExtAudioFile features and platform audio frameworks) without availability guards.
