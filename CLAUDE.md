# CLAUDE.md

## Project
Real-time C++ audio engine library for macOS and Linux; embedded by host applications via C or C++ APIs, controlled over OSC, and extended via Plugins.

## Stack
- C++17, C99 (public API)
- CMake with presets (`debug` / `release`)
- RtAudio (desktop audio driver), DummyDriver (headless/offline)
- macOS, Linux

## Commands
- Configure: `cmake --preset debug`
- Build: `cmake --build --preset debug`
- Test: `ctest --preset debug`
- Test (one): `ctest --preset debug -R <name>`
- Format: `pre-commit run --all-files`

## Architecture
- `include/methcla/` → public C/C++ API headers
- `src/Methcla/` → engine implementation
- `platform/` → platform-specific audio drivers (rtaudio, jack, ios, android)
- `plugins/` → built-in Plugin implementations (SynthDefs and Soundfile APIs)
- `tests/` → test suite
- `cmake/` → CMake modules and helpers
- `docs/` → architecture docs, OSC API reference, agent guides
- `external_libraries/` → vendored third-party dependencies (do not edit)
- `examples/` → example applications
- `scripts/` → release and utility scripts

## Rules
- IMPORTANT: Follow [coding-style.md](docs/agents/coding-style.md) for all code changes
- Use domain vocabulary from [CONTEXT.md](CONTEXT.md); avoid listed synonyms

## Workflow
- Format, build, and test before every commit
- Create PRs for non-trivial changes; commit directly to `develop` for small/low-risk changes
- PR branch names: short, descriptive, no prefixes, dashes (`my-awesome-feature`)
- When merging a PR via `gh`, pull and delete the branch
- Add a [CHANGELOG.md](CHANGELOG.md) entry under `[Unreleased]` for every feature or fix
- See [releases.md](docs/agents/releases.md) when releasing

## Out of scope
- Anything listed in `.gitignore` (build artifacts, generated files)
- `external_libraries/` — vendored third-party dependencies

## Human approval required
- Releases (version bumps, tagging, pushing tags)
- Modifications to the public API (`include/methcla/`)
