# Methcla usage guide

A practical reference for embedding Methcla in a host application using the C++ API (`methcla/engine.hpp`). Covers Engine setup, the request/bundle model, Synth lifecycle, AudioBus routing, and notifications.

See `CONTEXT.md` for domain terminology and `docs/osc-api.md` for the underlying OSC messages.

---

## Engine setup

```cpp
#include <methcla/engine.hpp>
#include <methcla/plugins/sine.h>

Methcla::EngineOptions options;

// Register built-in Plugins statically.
options.addLibrary(methcla_plugins_sine);

// Or load Plugins from a directory at startup.
options.addPluginDirectories({"/usr/lib/methcla/plugins"});

// Tune capacity to your needs (defaults shown).
options.realtimeMemorySize = 1024 * 1024;
options.maxNumNodes        = 1024;
options.maxNumAudioBuses   = 128;

Methcla::Engine engine(options);
engine.start();

// ... use engine ...

engine.stop();
```

`Engine::start()` / `stop()` start and stop the Driver. The Engine object itself is the interface for all host-side operations.

### Audio driver options

```cpp
options.audioDriver.sampleRate  = 48000;
options.audioDriver.numOutputs  = 2;
options.audioDriver.bufferSize  = 256;
```

Unset fields use the Driver's defaults. Pass a custom `Methcla_AudioDriver*` as the second argument to the `Engine` constructor to bypass the default Driver entirely.

---

## The request/bundle model

All commands to the Engine are sent as OSC packets. The C++ API wraps this with `Methcla::Request`.

### Single message

Each `EngineInterface` method (e.g. `engine.synth(...)`, `engine.free(...)`) sends one message immediately:

```cpp
engine.free(synthId);
```

### Bundle

Multiple messages sent atomically, optionally at a scheduled time:

```cpp
Methcla::Request request(engine);
request.openBundle(Methcla::immediately);  // or a future Methcla_Time
auto synth = request.synth(...);
request.activate(synth);
request.mapOutput(synth, 0, Methcla::AudioBusId(0), Methcla::kBusMappingExternal);
request.closeBundle();
request.send();
```

All messages in a bundle are processed atomically by the Engine in the same audio block. Use `engine.currentTime()` to get the current Engine time, then add a latency offset for scheduling into the future:

```cpp
const Methcla_Time kLatency = 0.003; // 3 ms
request.openBundle(engine.currentTime() + kLatency);
```

Bundles can be nested to interleave "now" setup with a future activation:

```cpp
request.openBundle(Methcla::immediately);
auto synth = request.synth(...);           // allocate now
request.mapOutput(synth, 0, bus, flags);   // wire up now
  request.openBundle(engine.currentTime() + kLatency);
  request.activate(synth);                 // activate later
  request.closeBundle();
request.closeBundle();
request.send();
```

There is also a lambda-based convenience overload on `EngineInterface`:

```cpp
engine.bundle(engine.currentTime() + kLatency, [&](Methcla::Request& r) {
    r.activate(synth);
});
```

---

## Node tree

The Engine maintains a tree of nodes (Groups and Synths). The root Group always exists at `engine.root()`.

### Groups

Groups are containers for organising Synths. Freeing a Group recursively frees all its children.

```cpp
Methcla::GroupId group = engine.group(engine.root());
// or within a bundle:
Methcla::GroupId group = request.group(engine.root());
```

`NodePlacement` controls where a new node is inserted:

| Constructor | Meaning |
|---|---|
| `NodePlacement(groupId)` | tail of group (default) |
| `NodePlacement::head(groupId)` | head of group |
| `NodePlacement::tail(groupId)` | tail of group |
| `NodePlacement::before(nodeId)` | immediately before node |
| `NodePlacement::after(nodeId)` | immediately after node |

### Freeing all children

```cpp
request.freeAll(group);  // free children, keep group alive
engine.free(group);      // free group and all children
```

---

## Synths

The built-in SynthDefs — their URIs, control port order, and OSC options — are documented in `plugins/README.md`. You need that information to call `request.synth(...)` correctly.

### Creating a Synth

```cpp
Methcla::SynthId synth = request.synth(
    METHCLA_PLUGINS_SINE_URI,   // SynthDef URI
    group,                       // NodePlacement
    {440.f, 0.5f},               // initial control values (one per control port, in order)
    {}                           // SynthDef-specific options (see plugin docs)
);
```

A Synth is created in a suspended state. It does not produce audio until activated.

### Activating

```cpp
request.activate(synth);
```

Always activate in the same bundle as `synth/new` or in a later timed bundle to compensate for I/O latency. A Synth that is never activated produces no output and holds its resources indefinitely.

### Setting control values

```cpp
engine.set(synth, 0, 880.0);  // port index 0, new value
// or in a bundle:
request.set(synth, 1, 0.25);  // port index 1
```

Port indices match the order declared in the SynthDef. See each plugin's documentation for the mapping.

---

## AudioBus routing

Synth audio ports are connected to AudioBuses with `mapInput` / `mapOutput`.

```cpp
// Route synth output 0 to internal bus 4.
request.mapOutput(synth, 0, Methcla::AudioBusId(4));

// Route internal bus 4 into another synth's input 0.
request.mapInput(otherSynth, 0, Methcla::AudioBusId(4));

// Route synth output 0 directly to hardware output channel 0.
request.mapOutput(synth, 0, Methcla::AudioBusId(0), Methcla::kBusMappingExternal);
```

### Bus mapping flags

| Flag | Meaning |
|---|---|
| `kBusMappingInternal` (default) | internal AudioBus |
| `kBusMappingExternal` | external (hardware) AudioBus |
| `kBusMappingFeedback` | for inputs: don't zero bus before reading (enables feedback loops with one-block delay) |
| `kBusMappingReplace` | for outputs: replace bus contents instead of mixing |

Flags can be combined with `|`.

### Allocating internal AudioBuses

The Engine allocates a pool of internal AudioBuses. Use `engine.audioBusId()` to manage IDs from the host:

```cpp
Methcla::AudioBusId bus = engine.audioBusId().alloc();
// ...use bus...
engine.audioBusId().free(bus);
```

---

## Synth lifecycle and done notifications

Some SynthDefs (sampler, disksampler, done-after, asr-envelope, exponential-fade) signal completion by calling `synthDone` internally. The Engine acts on that signal according to per-Synth **done flags** set by the host.

### Done flags

Configure done flags with `request.whenDone(synth, flags)` in the same bundle as Synth creation:

```cpp
request.whenDone(synth, Methcla::kNodeDoneFreeSelf | Methcla::kNodeDoneNotify);
```

| Flag | Effect when synthDone fires |
|---|---|
| `kNodeDoneDoNothing` (default) | nothing — Synth goes silent but stays allocated |
| `kNodeDoneFreeSelf` | free the Synth |
| `kNodeDoneFreePreceding` | free the node immediately before this one in the group |
| `kNodeDoneFreefollowing` | free the node immediately after |
| `kNodeDoneFreeAllSiblings` | free all other nodes in the same group |
| `kNodeDoneFreeParent` | free the parent group |
| `kNodeDoneNotify` | send a `/node/done` notification to the host |

Flags can be combined with `|`. Without `kNodeDoneFreeSelf`, a finished Synth stays in the tree and consumes a node slot.

### Notifications

The Engine sends two OSC notifications back to the host:

- `/node/done i:nodeId` — fired when `synthDone` fires with `kNodeDoneNotify` set.
- `/node/ended i:nodeId` — fired unconditionally when any node is freed (by any means).

Register a handler on `Engine`:

```cpp
engine.addNotificationHandler(
    Methcla::Engine::nodeDoneHandler(synth, [](Methcla::NodeId id) {
        // called when synth signals done (kNodeDoneNotify must be set)
    })
);

engine.addNotificationHandler(
    Methcla::Engine::nodeEndedHandler(synth, [](Methcla::NodeId id) {
        // called when synth is freed (by any means)
    })
);
```

A handler returns `true` to unregister itself after firing, or `false` to stay registered. The built-in helpers (`nodeDoneHandler`, `nodeEndedHandler`) return `true` on match — they fire once and remove themselves.

To free the node ID back to the allocator automatically when a node ends:

```cpp
engine.addNotificationHandler(engine.freeNodeIdHandler(synth));
// or with a callback:
engine.addNotificationHandler(engine.freeNodeIdHandler(synth, [bus1, bus2, &engine](Methcla::NodeId) {
    engine.audioBusId().free(bus1);
    engine.audioBusId().free(bus2);
}));
```

To remove a handler manually:

```cpp
auto handlerId = engine.addNotificationHandler(...);
engine.removeNotificationHandler(handlerId);
```

### Typical one-shot playback pattern

```cpp
Methcla::AudioBusId bus1 = engine.audioBusId().alloc();
Methcla::AudioBusId bus2 = engine.audioBusId().alloc();

Methcla::Request request(engine);
request.openBundle(Methcla::immediately);

// Sampler writes to internal buses.
auto sampler = request.synth(METHCLA_PLUGINS_SAMPLER_URI, group, {1.f, 1.f},
                              {Methcla::Value("/path/to/file.wav")});
request.mapOutput(sampler, 0, bus1);
request.mapOutput(sampler, 1, bus2);

// Patch cables route to hardware outputs and free themselves when the
// sampler finishes (kNodeDoneFreePreceeding propagates done upward).
auto cable1 = request.synth(METHCLA_PLUGINS_PATCH_CABLE_URI, group, {});
request.mapInput(cable1, 0, bus1);
request.mapOutput(cable1, 0, Methcla::AudioBusId(0), Methcla::kBusMappingExternal);
request.whenDone(cable1, Methcla::kNodeDoneFreeSelf | Methcla::kNodeDoneFreePreceeding);

auto cable2 = request.synth(METHCLA_PLUGINS_PATCH_CABLE_URI, group, {});
request.mapInput(cable2, 0, bus2);
request.mapOutput(cable2, 0, Methcla::AudioBusId(1), Methcla::kBusMappingExternal);
request.whenDone(cable2, Methcla::kNodeDoneFreeSelf);

request.openBundle(engine.currentTime() + kLatency);
request.activate(sampler);
request.activate(cable1);
request.activate(cable2);
request.closeBundle();

request.closeBundle();
request.send();

// Free node IDs and buses when everything is torn down.
engine.addNotificationHandler(engine.freeNodeIdHandler(sampler));
engine.addNotificationHandler(engine.freeNodeIdHandler(cable2, [&engine, bus1, bus2](Methcla::NodeId) {
    engine.audioBusId().free(bus1);
    engine.audioBusId().free(bus2);
}));
```

---

## Freeing nodes explicitly

```cpp
engine.free(synthId);   // free a single Synth
engine.free(groupId);   // free a Group and all its children recursively
```

Or in a bundle:

```cpp
request.free(nodeId);
```

`request.free` also returns the node ID to the host-side allocator immediately. When freeing from `engine.free` (outside a bundle), the ID is returned at send time.
