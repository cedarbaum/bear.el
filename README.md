# bear.el

<p align="center">
  <img src="./icons/bear-el-logo.png" width="256" />
</p>

## Introduction

This package allows you to browse and open notes you have written in [Bear](https://bear.app/). It is built on top of `markdown-mode` while also providing some additional syntax highlighting for Bear's flavor of markdown (e.g., backlinks and tags). There is some experiemental support for creating and updating note content, but this is turned off by default for safety (more details [here](#creating-and-updating-notes)).

## Installation

This package is not currently on MELPA, but can be installed using package managers that support installation directly from GitHub repos.

### Using `use-package` and [straight.el](https://github.com/radian-software/straight.el)

```elisp
(use-package bear
  :straight (:host github :repo "cedarbaum/bear.el"))
```

## Commands

### Opening and reading notes

- `bear-open-note`: Interactively open a note.
- `bear-pull-note`: Pulls the latest version of the currently open note from Bear and replaces the current buffer content with it.

### Creating and updating notes

> [!CAUTION]
> Commands that write to the database are considered experimental and unsafe. If you choose to use them:
> - Ensure you backup your notes and/or the local DB.
> - Close the Bear app before running the commands.
> - For the notes to sync properly, they need to be edited in the Bear app after creation or editing.
> - For these commands to work, `bear-readonly` must be set to `nil`.

- `bear-create-note`: Interactively create a new note with a given title
- `bear-push-note`: Pushes the content of the currently open note to Bear.

## Configuration

- `bear-application-data-url`: The path to Bear's SQLite database. This defaults to `~/Library/Group Containers/9K33E3U3T4.net.shinyfrog.bear/Application Data/` (see: https://bear.app/faq/where-are-bears-notes-located/)
- `bear-format-function`: An optional formatting function to run whenever a note is open. For example, you could run [Prettier](https://prettier.io/) to format the markdown.
- `bear-readonly`: Whether bear.el only operates in readonly mode. Defaults to `t`.
