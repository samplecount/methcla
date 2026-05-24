# Contributing

## Setup

Install the pre-commit hooks after cloning:

```sh
pip install pre-commit   # or: brew install pre-commit
pre-commit install
```

Hooks run automatically on `git commit`. To format all files manually:

```sh
pre-commit run --all-files
```

## Build and test

```sh
cmake --preset debug         # or: release
cmake --build build/debug    # or: build/release
ctest --test-dir build/debug --output-on-failure
```

Always build and run tests before submitting changes.

## Workflow

- **Non-trivial changes**: open a pull request. This lets CI run and allows review before merging.
- **Small, low-risk changes**: commit directly to `develop`.
- **Branch names**: short, descriptive, dashes. No prefixes. Example: `fix-rtaudio-latency`.
- **Merging**: use squash or merge commit via GitHub. Delete the branch after merging.

## Code style

See [`docs/coding-style.md`](docs/coding-style.md). C++17, C99. No new dependencies without discussion.

## Changelog

Add an entry under `## [Unreleased]` in `CHANGELOG.md` for every feature, fix, or notable change,
using the standard sections: Added, Changed, Deprecated, Removed, Fixed, Security.
