# Methcla

## Language

**Engine**:
The core audio processing runtime. Instantiated by a host application, runs an audio thread, manages Synths on AudioBuses, and loads Plugins.
_Avoid_: audio server, sound engine, runtime

**Driver**:
A platform-specific audio I/O backend that feeds samples to and from the Engine. RtAudio is the desktop Driver; DummyDriver is used for headless or offline use.
_Avoid_: audio backend, audio device

**Plugin**:
A shared library (`.so`/`.dylib`) that registers one or more SynthDefs with the Engine at load time. Loaded either dynamically from a directory or registered statically via function pointer.
_Avoid_: extension, module (conflicts with CMake MODULE library type)

**SynthDef**:
A synthesizer definition registered by a Plugin. Describes the processing graph and parameters for one unit of audio computation.
_Avoid_: instrument, unit generator, UGen

**Synth**:
A running instance of a SynthDef inside the Engine, allocated on an AudioBus.
_Avoid_: voice, instance, node (overloaded with the Node base class)

**AudioBus**:
A multi-channel buffer used for audio routing between Synths inside the Engine.
_Avoid_: channel, bus (acceptable shorthand in code)

**Resource**:
A Plugin-registered shared data object managed by the Engine, identified by a `Methcla_ResourceId` integer and typed by URI. Allocated/freed via OSC, acquired/released by Synths on the RT thread, accessed from the NRT context via `perform_with_resources`.
_Avoid_: instance (a Synth is an instance; a Resource is the thing the Plugin allocates), buffer (overloaded)

**ResourceDef**:
A Resource type registered by a Plugin. Describes the configure, construct, destroy callbacks and the interface URI for one kind of shared Resource.
_Avoid_: resource class, resource type

**Soundfile API**:
A Plugin category that provides file I/O capabilities to other Plugins (disksampler, sampler). Multiple soundfile API Plugins may be registered; the Engine selects by capability. Platform implementations: ExtAudioFile (macOS), libsndfile (Linux).
_Avoid_: audio file backend, file reader
