# Contributing

## Build and test

```sh
cmake --preset debug
cmake --build build/debug
ctest --test-dir build/debug --output-on-failure
```

Always build and run tests before submitting changes.

## Workflow

- **Non-trivial changes**: open a pull request. This lets CI run and allows review before merging.
- **Small, low-risk changes**: commit directly to `develop`.
- **Branch names**: short, descriptive, dashes. No prefixes. Example: `fix-rtaudio-latency`.
- **Merging**: use squash or merge commit via GitHub. Delete the branch after merging.

## Code style

Match the style of the surrounding code. C++17, C99. No new dependencies without discussion.
