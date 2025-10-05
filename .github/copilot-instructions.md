# Copilot Instructions: doom.d (Doom Emacs Config)

## Overview
Personal Doom Emacs configuration (~572KB, 30 `.el` files, 2,962 lines). **NOT a standalone app** - requires Doom Emacs framework installed at `~/.emacs.d/` and this config at `~/.doom.d/` or `~/.config/doom/`. Target: Emacs 29+. Custom modules: AI/LLM integration (gptel, copilot, MCP), git workflows (magit, forge, pr-review), LSP/tree-sitter support for 15+ languages.

## Build & Validation (Requires Doom Emacs Installed)

**CRITICAL:** All validation requires `~/.emacs.d/bin/doom` CLI. If unavailable, only check Elisp syntax.

### Validation Commands (in order)
1. **`~/.emacs.d/bin/doom sync`** - ALWAYS run after modifying `init.el`, `packages.el`, or package list. Installs/removes packages (30-120s). Failures usually = network issues or invalid package pins.
2. **`~/.emacs.d/bin/doom doctor`** - Check environment health. Custom checks: `git-imerge`, `pandoc`, `terminal-notifier`, fonts (JetBrainsMono, Overpass), `ollama`. Warnings OK if tools unused.
3. **`emacs --debug-init`** - Test configuration loads without errors.

### Without Doom CLI (syntax-only validation)
```bash
emacs --batch --eval "(check-parens)" file.el  # Check balanced parens
emacs --batch -l file.el                        # Test loading
```
Verify: matching parens, `lexical-binding: t` header, no undefined functions, proper Doom macros (`after!`, `use-package!`, `map!`).

## Architecture & File Structure

### Root Files (Load Order)
1. `init.el` → Doom module selection (`:lang`, `:tools`, `:user ai`, `:user git`, etc.)
2. `config.el` → Entry point, loads: `+core.el` (package configs), `+bindings.el` (keybinds), `+org.el` (org-mode), `+packages.el` (use-package blocks)
3. `packages.el` → Package declarations: `(package! name :pin "hash")`
4. `doctor.el` → Custom health checks

### Key Directories
- **`autoload/*.el`** - Lazy-loaded functions (must have `;;;###autoload` cookie). Files: `+bindings`, `+buffer`, `+capf`, `+comments`, `+cpp`, `+flyspell`, `+lsp`, `+org`, `+shell`, `+text`, `+theme`
- **`modules/user/ai/`** - AI module: gptel, copilot, MCP, chatgpt-shell configs. Requires optional `ollama` for local LLMs
- **`modules/user/git/`** - Git module: magit, forge, pr-review, git-link configs. Requires optional `git-imerge`
- **`prompts/`** - GPTel prompt templates (`emacs-development.org` = coding guidelines)
- **`snippets/`** - YASnippet templates by mode

### Common Patterns
**Add Package:** `packages.el` → `(package! name :pin "hash")` → `doom sync`
**Configure:** `+core.el` (general), `+bindings.el` (keys), `+packages.el` (use-package blocks), or module `config.el`
**Autoload Function:** Create in `autoload/+NAME.el` with `;;;###autoload` → `doom sync`
**Add Module:** `init.el` → enable under `:user` section → `doom sync`

## Critical Pitfalls & Solutions

1. **Missing lexical-binding header** - ALL `.el` files MUST start with `;;; path/file.el -*- lexical-binding: t; -*-`
2. **Package sync not run** - After editing `init.el` or `packages.el`, ALWAYS run `doom sync`. Missing this = undefined packages.
3. **Autoload not regenerated** - After adding `;;;###autoload` functions, run `doom sync`. Functions won't be found otherwise.
4. **Invalid package pin** - Package install fails if pin hash invalid. Try removing `:pin` or updating to latest commit.
5. **Custom module not loaded** - Ensure module listed in `init.el` under `:user` section and has both `packages.el` and `config.el`.
6. **API keys missing** - AI features require auth-source config: Google/Claude/OpenAI API keys. Failures expected if unconfigured.
7. **Font warnings** - `doom doctor` warns about JetBrainsMono/Overpass fonts. Safe to ignore on non-GUI or different fonts.

## Code Conventions (REQUIRED)

**Naming:** `+bl/function-name` (user), `+bl/fn--private` (internal), `+bl/fn-a` (advice), `+bl/fn-h` (hook), `+bl/fn-p` (predicate)
**Headers:** `;;; -*- lexical-binding: t; -*-` (line 1), `;;;` (section headers), `;;` (inline comments)
**Use-package:** `:defer t` (lazy load), `:commands` (autoload), `:init` (before load), `:config` (after load)
**Keybinds:** Use `map!` macro: `(map! :leader :desc "X" "k" #'func)` or `(:prefix "g" :desc "Git" "g" #'magit)`
**Paths:** Use `doom-user-dir`, `doom-local-dir`, `expand-file-name` - never hardcode paths

## Quick Reference

**Config Files:** `init.el` (modules), `packages.el` (package list), `+core.el` (settings), `+bindings.el` (keys), `+org.el` (org), `+packages.el` (use-package blocks)
**Modules:** `modules/user/ai/` (AI/LLM), `modules/user/git/` (git workflows)
**Autoloads:** `autoload/*.el`, `modules/user/*/autoload/*.el` (need `;;;###autoload` cookie)
**Health Checks:** `doctor.el`, `modules/user/*/doctor.el`
**No CI/CD:** No workflows, no automated testing. Personal config for Björn Larsson.

**Machine-local overrides:** `~/.doom.local.el` (config), `~/.packages.local.el` (packages) - loaded if present.

**When unsure:** Follow existing patterns. Look at similar configs in same file. Use Doom macros: `after!`, `use-package!`, `map!`, `set-popup-rule!`. Trust the self-documenting structure.
