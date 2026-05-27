# Methcla OpenSoundControl API

Methcla uses OpenSoundControl (OSC) as a serialisation format for commands sent from one or more application threads to the engine. This allows to keep the C API small and doesn't require packing C structures from other programming languages, which can be difficult and non-portable.

## Relevant C API functions

    Methcla_Error methcla_engine_send(Methcla_Engine* engine, const void* packet, size_t size);

Send an OSC packet (data and size) to the engine.

    Methcla_Time methcla_engine_current_time(const Methcla_Engine* engine);

Get the current engine time as a `Methcla_Time` value (currently double precision float in seconds).

    uint64_t methcla_time_to_uint64(Methcla_Time time);

Encode a `Methcla_Time` value as a 64 bit unsigned integer for use as an OSC bundle timestamp.

## Node placement values (`target-spec`)

Used by **`/group/new`** and **`/synth/new`** to specify where a new node is inserted relative to `target-id`:

| Constant | Meaning |
|---|---|
| `kMethcla_NodePlacementHeadOfGroup` | head of target group |
| `kMethcla_NodePlacementTailOfGroup` | tail of target group |
| `kMethcla_NodePlacementBeforeNode` | immediately before target node |
| `kMethcla_NodePlacementAfterNode` | immediately after target node |

## Commands

* **`/group/new`** `i:node-id i:target-id i:target-spec`

  Create a new group with id `node-id` and insert it into the node tree relative to `target-id` according to `target-spec`.

* **`/group/freeAll`** `i:node-id`

  Free all child nodes of the group `node-id`, keeping the group itself alive.

* **`/synth/new`** `s:definition-name i:node-id i:target-id i:target-spec [f:synth-controls] [synth-options]`

  Create a new synth with id `node-id` from the synth definition `definition-name` and insert it into the node tree relative to `target-id` according to `target-spec`. `synth-controls` is an array of initial control values; its length must match the number of control inputs provided by the synth. `synth-options` is an array of options passed to the synth constructor; it may be empty and its interpretation depends on the synth definition.

* **`/synth/activate`** `i:node-id`

  Activate a synth after it has been created. In order to produce output, each **`/synth/new`** *must* be followed by **`/synth/activate`**. The intention is to be able to do useful asynchronous work (such as loading a soundfile) in the synth constructor by performing **`/synth/new`** instantly and scheduling **`/synth/activate`** into the future by the desired amount so as to compensate for the I/O latency and jitter.

* **`/synth/map/input`** `i:node-id i:index i:bus-id i:flags`

  Map a synth's audio input `index` to `bus-id`. Flags may be one of

  * `kMethcla_BusMappingInternal = 0x00`

     Map to an internal audio bus.

  * `kMethcla_BusMappingExternal = 0x01`

     Map to an external (hardware) audio bus.

  * `kMethcla_BusMappingFeedback = 0x02`

     Don't zero bus before reading, allowing feedback loops with a delay of one block size.

  * `kMethcla_BusMappingReplace  = 0x04`

     Zero bus before reading (default).

* **`/synth/map/output`** `i:node-id i:index i:bus-id i:flags`

  Map a synth's audio output `index` to `bus-id`. Flags may be one of

  * `kMethcla_BusMappingInternal = 0x00`

     Map to an internal audio bus.

  * `kMethcla_BusMappingExternal = 0x01`

     Map to an external (hardware) audio bus.

  * `kMethcla_BusMappingFeedback = 0x02`

     Mix output with previous bus contents (default).

  * `kMethcla_BusMappingReplace  = 0x04`

     Replace bus contents by output.

* **`/synth/property/doneFlags/set`** `i:node-id i:flags`

  Set the done-action flags for a synth. `flags` is a bitmask of:

  | Constant | Effect when `synthDone` fires |
  |---|---|
  | `kMethcla_NodeDoneDoNothing` | nothing — synth goes silent but stays allocated |
  | `kMethcla_NodeDoneFreeSelf` | free this synth |
  | `kMethcla_NodeDoneFreePreceeding` | free the node immediately before this one |
  | `kMethcla_NodeDoneFreeFollowing` | free the node immediately after this one |
  | `kMethcla_NodeDoneFreeAllSiblings` | free all other nodes in the same group |
  | `kMethcla_NodeDoneFreeParent` | free the parent group |
  | `kMethcla_NodeDoneNotify` | send a **`/node/done`** notification to the host |

* **`/node/free`** `i:node-id`

  Free a node and all associated resources. Freeing a group frees all its children recursively.

* **`/node/set`** `i:node-id i:index f:value`

  Set a synth's control input at `index` to the specified value.

## Query messages

These messages take a `request-id` and return a response message at the same address. On error the engine responds with **`/error`** instead (see below).

* **`/node/tree/statistics`** `i:request-id`

  Request node tree statistics. Response: **`/node/tree/statistics`** `i:num-groups i:num-synths`

* **`/engine/realtime-memory/statistics`** `i:request-id`

  Request realtime memory statistics. Response: **`/engine/realtime-memory/statistics`** `i:free-bytes i:used-bytes`

## Notifications

The engine sends these messages to the host without a corresponding request. They arrive on the packet handler registered via `methcla_engine_options_set_packet_handler` with `requestId == kMethcla_Notification`.

* **`/node/done`** `i:node-id`

  Sent when a synth signals completion (`synthDone`) and `kMethcla_NodeDoneNotify` is set in its done flags.

* **`/node/ended`** `i:node-id`

  Sent unconditionally when any node is freed (by any means, including **`/node/free`**, done-action flags, or parent group teardown).

## Error responses

* **`/error`** `i:error-code s:message`

  Returned in place of the normal response when a query fails, or dispatched as a notification when a command fails. `error-code` is a `Methcla_ErrorCode` value.
