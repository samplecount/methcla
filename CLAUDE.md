## Agent skills

### Issue tracker

Issues live in GitHub Issues for `samplecount/methcla`. See `docs/agents/issue-tracker.md`.

### Triage labels

Uses the default five-role vocabulary (needs-triage, needs-info, ready-for-agent, ready-for-human, wontfix). See `docs/agents/triage-labels.md`.

### Domain docs

Single-context layout: one `CONTEXT.md` + `docs/adr/` at the repo root. See `docs/agents/domain.md`.

## GitHub workflows

- Create PRs for non-trivial changes. This allows reviewing code and running actions before merging.
- Create PR branches before submitting a PR or when prompted to submit changes to a PR branch.
- For small, low-risk changes, or when prompted to do so, commit directly to develop.
- Before committing changes or submitting PRs, build locally and run tests.
- When merging PRs via gh, update the working copy accordingly (pull and delete PR branch).
- Keep PR branch name descriptive but short. No prefixes like `feat/`. Dashes: `my-awesome-new-feature`.
