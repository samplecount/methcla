# Methcla OpenSoundControl API

Methcla uses OpenSoundControl (OSC) as a serialisation format for commands sent from one or more application threads to the engine. This allows to keep the C API small and doesn't require packing C structures from other programming languages, which can be difficult and non-portable.

## Relevant C API functions

    Methcla_Error methcla_engine_send(Methcla_Engine* engine, const void* packet, size_t size);

Send an OSC packet (data and size) to the engine.

    Methcla_Time methcla_engine_current_time(const Methcla_Engine* engine);

Get the current engine time as a `Methcla_Time` value (currently double precision float in seconds).

    uint64_t methcla_time_to_uint64(Methcla_Time time);

Encode a `Methcla_Time` value as a 64 bit unsigned integer for use as an OSC bundle timestamp.

## OSC API

* `/group/new i:node-id i:target-id i:target-spec`

  Create a new group with id `node-id` and insert it into the group with id `target-id` according to `target-spec`. **NOTE**: `target-spec` is currently ignored, new groups are always placed at the tail of the target group.

* `/synth/new s:definition-name i:node-id i:target-id i:target-spec [f:synth-controls] [synth-options]`

  Create a new synth with id `node-id` from the synth definition `definition-name` and insert it into the group with id `target-id` according to `target-spec`. `synth-controls` is an array of initial control values; its length must match the number of control inputs provided by the synth. `synth-options` is an array of options passed to the synth constructor; it may be empty and its interpretation depends on the synth definition.

  **NOTE**: `target-spec` is currently ignored, new groups are always placed at the tail of the target group.

* `/synth/activate i:node-id`

  Activate a synth after it has been created. In order to produce output, each `/synth/new` *must* be followed by `/synth/activate`. The intention is to be able to do useful asynchronous work (such as loading a soundfile) in the synth constructor by performing `/synth/new` instantly and scheduling `/synth/activate` into the future by the desired amount so as to compensate for the I/O latency and jitter.

* `/synth/map/input i:node-id i:index i:bus-id i:flags`

  Map a synth's audio input `index` to `bus-id`. Flags may be one of

  * `kMethcla_BusMappingInternal = 0x00`

     Map to an internal audio bus.

  * `kMethcla_BusMappingExternal = 0x01`

     Map to an external (hardware) audio bus.

  * `kMethcla_BusMappingFeedback = 0x02`

     Don't zero bus before reading, allowing feedback loops with a delay of one block size.
  
  * `kMethcla_BusMappingReplace  = 0x04`
  
     Zero bus before reading (default).

* `/synth/map/output i:node-id i:index i:bus-id i:flags`

  Map a synth's audio output `index` to `bus-id`. Flags may be one of

  * `kMethcla_BusMappingInternal = 0x00`

     Map to an internal audio bus.

  * `kMethcla_BusMappingExternal = 0x01`

     Map to an external (hardware) audio bus.

  * `kMethcla_BusMappingFeedback = 0x02`

     Mix output with previous bus contents (default).
  
  * `kMethcla_BusMappingReplace  = 0x04`
  
     Replace bus contents by output.

* `/node/free` i:node-id

  Free a node and all associated resources. Freeing a group frees all its children recursively.

* `/node/set` i:node-id i:index f:value

  Set a synth's control input at `index` to the specified value.
