#!/usr/bin/env python3
"""Bump version, update changelog, commit, and tag a release."""

import re
import subprocess
import sys
from pathlib import Path


def run(cmd):
    subprocess.run(cmd, check=True)


def get_output(cmd):
    return subprocess.run(cmd, check=True, capture_output=True, text=True).stdout.strip()


def github_url():
    remote = get_output(["git", "remote", "get-url", "origin"])
    remote = re.sub(r"\.git$", "", remote)
    remote = re.sub(r"^git@github\.com:", "https://github.com/", remote)
    return remote


def bump_cmake(root: Path, version: str):
    path = root / "CMakeLists.txt"
    updated, n = re.subn(
        r"(project\(methcla VERSION )\S+(\))",
        rf"\g<1>{version}\2",
        path.read_text(),
    )
    if not n:
        sys.exit("Error: could not find version in CMakeLists.txt")
    path.write_text(updated)


def update_changelog(root: Path, version: str, repo_url: str) -> str:
    path = root / "CHANGELOG.md"
    text = path.read_text()

    prev = re.search(r"^## \[(\d+\.\d+\.\d+)\]", text, re.MULTILINE)
    if not prev:
        sys.exit("Error: could not find previous release version in CHANGELOG.md")
    prev_version = prev.group(1)

    # Insert new version heading after [Unreleased]
    text = text.replace(
        "## [Unreleased]\n",
        f"## [Unreleased]\n\n## [{version}]\n",
        1,
    )

    # Update [unreleased] comparison link
    text = re.sub(
        r"(?im)^\[unreleased\]: \S+",
        f"[unreleased]: {repo_url}/compare/v{version}...HEAD",
        text,
    )

    # Insert new version link above the previous version's link
    text = text.replace(
        f"[{prev_version}]:",
        f"[{version}]: {repo_url}/compare/v{prev_version}...v{version}\n[{prev_version}]:",
        1,
    )

    path.write_text(text)
    return prev_version


def main():
    if len(sys.argv) != 2 or not re.fullmatch(r"\d+\.\d+\.\d+", sys.argv[1]):
        sys.exit("Usage: tools/release.py <version>  (e.g. 1.2.3)")

    version = sys.argv[1]
    root = Path(get_output(["git", "rev-parse", "--show-toplevel"]))
    repo_url = github_url()

    bump_cmake(root, version)
    prev_version = update_changelog(root, version, repo_url)

    run(["git", "add", "CMakeLists.txt", "CHANGELOG.md"])
    run(["git", "commit", "-m", f"Release v{version}"])
    run(["git", "tag", "-a", f"v{version}", "-m", f"Release v{version}"])

    print(f"\nReleased v{version} (previous: v{prev_version})")
    print("Push with:\n  git push && git push --tags")


if __name__ == "__main__":
    main()
