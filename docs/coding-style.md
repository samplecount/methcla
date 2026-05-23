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

- Header guards: `#ifndef METHCLA_<PATH>_HPP_INCLUDED` / `#define` / `#endif`.
- Include order: own header first, then internal headers, then third-party, then standard library. Each group separated by a blank line.

### Namespaces

- `Methcla::` for C++ API.
- Anonymous namespace for translation-unit-local helpers instead of `static`.

### Formatting

- 4-space indentation, no tabs.
- Allman brace style for functions and control flow (opening brace on its own line).
- Braces on the same line for namespace and class bodies.

## C API

- All public symbols prefixed `methcla_` (functions) or `Methcla_` (types).
- Enum constants prefixed `kMethcla_`.
- `typedef struct` and `typedef enum` — no anonymous structs.
- `extern "C"` guards in every public header.
- `METHCLA_EXPORT` on all exported symbols.
- `snake_case` for all identifiers.
- Header guards: `#ifndef METHCLA_<NAME>_H_INCLUDED`.

## Python

- Type annotations on all function signatures.
- `pathlib.Path` for file paths, not string concatenation.
- `subprocess.run(..., check=True)` — don't silently swallow non-zero exit codes.
- Keep scripts short and single-purpose. Extract helpers only when reused.
