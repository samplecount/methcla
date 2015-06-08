### 0.3.0

* Add playback rate control to disksampler
* Add node placement options to node creation API commands. `Methcla::NodePlacement` can be used to control node placement in the C++ API.
* Remove `Methcla_Resource` from plugin API: Remove argument from `Methcla_SynthDef::construct` and rename `methcla_world_resource_retain`/`methcla_world_resource_release` to `methcla_world_synth_retain`/`methcla_world_synth_release`
* Implement various node placement options
* Allow plugins to notify the engine that they are done processing; add `methcla_world_synth_done` function to plugin API
* Fix bus zeroing logic (#102)
* Add operator bool to `Methcla::NodeId`
* Add `Methcla::Engine::nodeEndedHandler` API method returning `/node/ended` notification handler

### 0.2.0

* Rename `Methcla::Engine::freeNode` to `Methcla::Engine::free`
* Use `Methcla::Engine::Bundle` instead of `Methcla::Engine::Request`
* Move plugin includes to `<methcla/plugins/*>`
* **[Pro]** Include ExtAudioFile soundfile API
* Split synth creation into `/synth/new` (`Methcla::Engine::synth`) and `/synth/activate` (`Methcla::Engine::activate`)
* Refactor engine interface: Rename `Methcla::Engine::Request` to `Methcla::Request` and remove `Methcla::Engine::Bundle`. `Methcla::Request` now has `openBundle`/`closeBundle` methods for creating (nested) bundle structures.
* Add function for querying library version (`methcla_version`, `Methcla::version()`)
* Add function for changing debug logging behaviour (`methcla_engine_set_log_flags`, `Methcla::Engine::setLogFlags`)
