# Releases

## Steps

1. Ensure `## [Unreleased]` in [`CHANGELOG.md`](../../CHANGELOG.md) is up to date.
2. Run `scripts/release.py <version>` (e.g. `scripts/release.py 1.2.3`). This bumps the version in [`CMakeLists.txt`](../../CMakeLists.txt), moves `[Unreleased]` content to `[version]` in [`CHANGELOG.md`](../../CHANGELOG.md), commits both files, and creates an annotated git tag.
3. Push: `git push && git push --tags`.
4. Create a GitHub release from the tag with `gh release create`. The description has two sections: `## A-side` containing a short rap, followed by `## B-side` containing the new version's changelog entry verbatim.

## Rap rules

- Up to four verses, four lines per verse.
- One emoji at the end of each line.
- Reference concrete changes from the changelog (commands, APIs, fixes), not generic flexing.
- Tone: playful, hip-hop bravado, technically accurate.
