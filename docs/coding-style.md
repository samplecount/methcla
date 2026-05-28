# Coding style

## General

- Avoid global state. Prefer passing dependencies explicitly.
- No magic numbers — use named constants or enums.
- Match the style of the surrounding code when in doubt.

## C++

### Naming

| Kind | Convention | Example |
|---|---|---|
| Types, classes | `PascalCase` | `NodeId`, `AudioBus` |
| Functions, methods | `camelCase` | `addToHead`, `isGroup` |
| Member variables | `m_` prefix | `m_first`, `m_engine` |
| Enum constants | `kMethcla_` prefix | `kMethcla_NodePlacementHeadOfGroup` |
| Macros | `METHCLA_` prefix | `METHCLA_EXPORT` |

### Types and variables

- Use `const` on variables, parameters, and member functions wherever possible.
- Use `constexpr` instead of `const` for compile-time constants.
- Use `nullptr`, not `NULL` or `0` for pointers.
- Use `size_t` for sizes and counts. Cast once at the boundary; don't scatter casts.
- Use `static_cast` for explicit conversions. Avoid C-style casts.

### Classes

- Mark overriding methods `override`. Do not also mark them `virtual`.

### Headers

- File header for new files: `// Copyright (C) <year> Methcla contributors` followed by `//` and `// SPDX-License-Identifier: Apache-2.0`.
- When editing a file that already has a copyright notice, add `// Copyright (C) <year> Methcla contributors` as an additional line immediately after the existing copyright line(s); keep the original notice. If the Methcla contributors line is already present, extend its year range to include the current year (e.g. `2026` → `2026-2027`).
- Use `#pragma once` instead of `#ifndef` include guards.
- Include order: own header first, then internal headers, then third-party, then standard library. Each group separated by a blank line.

### Control flow

- Prefer `else` branches over early returns.

### Namespaces

- `Methcla::` for C++ API.
- Use C++17 compact nested namespace syntax.
- Anonymous namespace for translation-unit-local helpers instead of `static`.

### Formatting

Enforced by clang-format. Configuration is in `.clang-format`. Run `pre-commit run --all-files` rather than formatting manually.

## C API

- All public symbols prefixed `methcla_` (functions) or `Methcla_` (types).
- Enum constants prefixed `kMethcla_`.
- `typedef struct` and `typedef enum` — no anonymous structs.
- `extern "C"` guards in every public header.
- `METHCLA_EXPORT` on all exported symbols.
- `snake_case` for all identifiers.
- Header guards: `#ifndef METHCLA_<NAME>_H_INCLUDED`.

## Testing

- Name tests as `Subject_Description` (e.g. `Methcla_Utility_Semaphore.TryWait_empty_returns_false`).
- Add a comment before each test describing the scenario and how it verifies the behaviour, unless the name makes it self-evident.

## Python

- Type annotations on all function signatures.
- `pathlib.Path` for file paths, not string concatenation.
- `subprocess.run(..., check=True)` — don't silently swallow non-zero exit codes.
- Keep scripts short and single-purpose. Extract helpers only when reused.

### Formatting

Enforced by black. Run `pre-commit run --all-files` rather than formatting manually.
