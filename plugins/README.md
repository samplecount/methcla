# plugins/

Built-in Plugins shipped with Methcla.

Several SynthDefs signal completion by calling `synthDone` internally (noted in their descriptions below). This is a hint to the Engine that the Synth has finished its work. What happens next — whether the Synth is freed, which other nodes are freed alongside it, and whether the host is notified — depends on per-Synth done flags configured by the host via `request.whenDone(...)`. See `docs/usage.md` for details.

## Audio SynthDefs

### sine (`sine.c`)

URI: `methcla:plugins/sine`

Simple band-limited sine wave oscillator. Maintains phase across blocks.

| Port | Kind | Direction |
|---|---|---|
| `freq` | control | in |
| `amp` | control | in |
| `out` | audio | out |

---

### patch-cable (`patch_cable.cpp`)

URI: `methcla:plugins/patch-cable`

Copies audio from input to output unchanged (1-in / 1-out passthrough). Useful for routing AudioBuses.

| Port | Kind | Direction |
|---|---|---|
| `in` | audio | in |
| `out` | audio | out |

The same file also registers **amplifier**:

URI: `methcla:plugins/amplifier`

Scales an audio input by a gain control value. Skips the multiply loop when `gain` is exactly `0.0` or `1.0`.

| Port | Kind | Direction |
|---|---|---|
| `input` | audio | in |
| `output` | audio | out |
| `gain` | control | in |

---

### sampler (`sampler.cpp`)

URI: `methcla:plugins/sampler`

Memory-based audio file player. On construction it dispatches an async host command to load the entire file into a heap buffer, then plays it back on the audio thread using 4-point Hermite interpolation at the given rate. Outputs silence until the buffer is loaded. Frees itself and signals `synthDone` when playback reaches the end (non-looping).

**OSC options** (passed at Synth creation): `path` (string), `loop` (int, default 0), `startFrame` (int, default 0), `numFrames` (int, default −1 = all).

| Port | Kind | Direction |
|---|---|---|
| `amp` | control | in |
| `rate` | control | in |
| `output_0` | audio | out |
| `output_1` | audio | out |

Requires a Soundfile API Plugin to be registered (see below).

---

### disksampler (`disksampler.cpp`)

URI: `methcla:plugins/disksampler`

Streaming disk-based audio file player. Uses a ring buffer with double-buffering: a background host thread fills transfer blocks (`kDiskTransferSize = 64 KB`) while the audio thread consumes them. Falls back to memory playback for very short files. Supports looping and variable-rate Hermite resampling.

Same ports and OSC options as **sampler**.

Requires a Soundfile API Plugin to be registered (see below).

---

## Lifecycle / envelope SynthDefs (`node_control.cpp`)

### done-after

URI: `methcla:plugins/done-after`

No-op Synth that calls `synthDone` after a given number of seconds. Useful for timed Synth cleanup.

**OSC options**: `seconds` (float).
No ports.

---

### asr-envelope

URI: `methcla:plugins/asr-envelope`

Attack–Sustain–Release amplitude envelope applied to an audio signal. Calls `synthDone` at the end of the release phase.

**OSC options**: `attackTime`, `sustainTime`, `sustainLevel`, `releaseTime` (all float, in seconds / linear amplitude).

| Port | Kind | Direction |
|---|---|---|
| `input` | audio | in |
| `output` | audio | out |

---

### exponential-fade

URI: `methcla:plugins/exponential-fade`

Multiplies an audio input by a level that ramps exponentially from `startLevel` to `endLevel` over `duration` seconds. Calls `synthDone` when the ramp finishes.

**OSC options**: `startLevel`, `endLevel`, `duration` (float).

| Port | Kind | Direction |
|---|---|---|
| `input` | audio | in |
| `output` | audio | out |

---

## Soundfile API Plugins

Soundfile API Plugins do not register SynthDefs. They call `methcla_host_register_soundfile_api` to provide file I/O to sampler and disksampler.

| File | When built | Backend |
|---|---|---|
| `soundfile_api_extaudiofile.cpp` | macOS only (`APPLE`) | Apple ExtAudioFile (CoreFoundation + AudioToolbox) |
| `soundfile_api_libsndfile.cpp` | when `libsndfile` found via pkg-config | libsndfile |
| `soundfile_api_dummy.cpp` | always | Returns random-length silence; for testing only |

---

## Implementing a new Plugin

Every Plugin exposes a single entry point:

```c
METHCLA_EXPORT Methcla_Library* METHCLA_PLUGIN_LOAD(<name>)(
    Methcla_Host* host, const char* bundlePath);
```

Inside it calls `methcla_host_register_synthdef` (or `methcla_host_register_soundfile_api` for Soundfile API Plugins) and returns a `Methcla_Library*`.

### Implementation styles

| Style | Used in |
|---|---|
| **C++ wrapper** (preferred) — subclass `StaticSynthDef<Synth, Options, Ports>` from `methcla/plugin.hpp` | `patch_cable.cpp`, `node_control.cpp`, `disksampler.cpp` |
| **C API** — fill `Methcla_SynthDef` struct with function pointers | `sine.c`, `sampler.cpp` |

Both styles produce identical runtime behaviour. The C++ wrapper reduces boilerplate for complex SynthDefs.

### Steps

1. Add a source file (`.c` or `.cpp`).
2. Add a public header under `include/methcla/plugins/` defining the URI macro.
3. Add the source to the `foreach` loop in `CMakeLists.txt` (or wire it up conditionally like the Soundfile API Plugins).

`CMakeLists.txt` defines a local `add_methcla_plugin(name source)` helper that creates a CMake `MODULE` target named `methcla_plugin_<name>`, output to `${CMAKE_BINARY_DIR}/plugins/`, installed to `${CMAKE_INSTALL_LIBDIR}/methcla/plugins/`.
