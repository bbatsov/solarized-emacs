# Changelog

## Unreleased

- [#462](https://github.com/bbatsov/solarized-emacs/pull/462): Expand face coverage to jinx, completion-preview, copilot, keycast, dictionary, mistty, easy-kill, vundo, asciidoc-mode, clojure-mode, haskell-mode, erlang, inf-ruby, gptel and breadcrumb, complete the anzu and git-timemachine faces, and deepen the cider coverage (REPL, stacktrace and nREPL log).

## 2.1.0 (2026-03-29)

### New features

- Add the `solarized-select-theme` command for picking a variant interactively via `completing-read`.
- Track the current theme through `enable-theme-functions` hooks (Emacs 29+), so `solarized-reload` reuses it automatically.

### New face definitions

- Full vertico coverage (current, group-title, group-separator, multiline).
- Comprehensive marginalia coverage (35 faces for annotations, file permissions, types and more).
- Full embark coverage (keybindings, targets, collect buffers, verbose indicator).
- Expand consult from 2 to 16 faces (files, bookmarks, async status, line numbers and so on).
- Expand corfu from 4 to 11 faces (annotations, deprecated, echo, popupinfo, quick keys, indexed).
- Full eglot coverage (highlighted symbol, diagnostics, mode-line, inlay hints).
- Expand which-key from 3 to 9 faces (separator, note, special-key, docstring and others).

### Bug fixes

- Fix the selenized-zenburn theme docstring, which incorrectly said "gruvbox".
- Fix a duplicate "The the" in the wombat-dark docstring.

## 2.0.5 (2025-02-22)

- [#452](https://github.com/bbatsov/solarized-emacs/pull/452): Add support for the built-in `tab-line` and `window-tool-bar` packages.
- [#453](https://github.com/bbatsov/solarized-emacs/pull/453): Match the flymake `error`, `warning` and `note` faces to their flycheck counterparts.
- [#451](https://github.com/bbatsov/solarized-emacs/pull/451): Highlight `font-lock-number-face` like `highlight-numbers-number`.

## 2.0.4 (2023-12-04)

- [#445](https://github.com/bbatsov/solarized-emacs/pull/445): Remove the `all-the-icons-dired-dir-face` definition.
- [#448](https://github.com/bbatsov/solarized-emacs/pull/448): Fix invalid `:style unspecified` box properties.

## 2.0.3 (2023-08-20)

- [#442](https://github.com/bbatsov/solarized-emacs/pull/442): Add a `corfu` face.

## 2.0.2 (2023-08-05)

- [#441](https://github.com/bbatsov/solarized-emacs/pull/441): Fix a byte-compiler warning.

## 2.0.1 (2023-05-07)

- [#433](https://github.com/bbatsov/solarized-emacs/pull/433): Add `ansi-color-*` faces (Emacs 28.1).
- [#435](https://github.com/bbatsov/solarized-emacs/pull/435): Use extra-light instead of thin for line numbers.
- [#439](https://github.com/bbatsov/solarized-emacs/pull/439): Update the terraform faces for the renamed faces in terraform-mode.

## 2.0.0 (2022-06-18)

- Major release with breaking API changes. The theme itself upgrades without issues; if you maintain a child theme, see the [upgrade guide](https://github.com/bbatsov/solarized-emacs/blob/master/UPGRADE-GUIDE.md).

Older releases are documented in the [GitHub releases](https://github.com/bbatsov/solarized-emacs/releases).
