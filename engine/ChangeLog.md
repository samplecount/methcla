* `Methcla::Engine::freeNode` renamed to `Methcla::Engine::free`
* Use `Methcla::Engine::Bundle` instead of `Methcla::Engine::Request`
* Plugin includes moved to `<methcla/plugins/*>`
* **[Pro]** Include ExtAudioFile soundfile API
* Synth creation split into `/synth/new` (`Methcla::Engine::synth`) and `/synth/activate` (`Methcla::Engine::activate`)
* Refactored engine interface: Renamed `Methcla::Engine::Request` to `Methcla::Request` and removed `Methcla::Engine::Bundle`. `Methcla::Request` now has `openBundle`/`closeBundle` methods for creating (nested) bundle structures.
* Add `methcla_version` and Methcla::version() API functions
