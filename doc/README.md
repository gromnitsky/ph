# Emacs Project Helper

ph is a global minor mode for easy opening/closing/switching groups of
related files (commonly called "projects").

![emacs ph menu](https://raw.github.com/gromnitsky/ph/master/doc/ss-menu.png)

For general design see `doc/structure.org`.

## Features

* Nothing to configure.
* Support any types of projects & any files.
* Ido-style project files switching.
* Any number of opened projects simultaneously.
* Auto-remembering/-forgetting what file belongs to a project.
* Open/close project files with 1 command.
* Doesn't touch your Emacs configs & doesn't write anywhere except 1
  special file in project directory.
* Detects file moving withing project sub-directories & outside of
  project directory.
* Menu for quick projects switching.

## Installation

`.emacs`:

    (add-to-list 'load-path "/directory/with/ph")

	(require 'ph)
    (ph-mode)

Optionally byte-compile it by typing

    % make compile

in the tarball directory.

## Quick Start

> `M-x` ph-project-new

And choose some directory with your pet project files. (It's not
necessary to kill buffers with those files in Emacs if they are already
opened--such buffers will be automatically marked as a project
belongings.)

> `C-x C-f` some-file-in-the-chosen-directory<br>
> `C-x C-f` another-file-in-the-chosen-directory<br>
> `C-x C-f` and-another-file-in-the-chosen-directory<br>

> `M-x` ph-project-switch-buffer

Nice, right?

> `M-x` ph-project-new

And choose some other directory with your other pet project files.

> `M-x` ph-project-switch

Nice, right?

Then

> `M-x` ph-project-close

Yay! Then switch to the previous project:

> `M-x` ph-project-switch

And

> `M-x` ph-project-close

Finally,

> `M-x` ph-project-open

And choose .ph file location.

## Requirements

### Running

* Linux/BSD
* Emacs 24.3

Tested only under Fedora 17 and Emacs 24.3.50.1 (2013-02-08) from trunk.
Won't work under Windows. No idea about OSX.

### Building

* GNU Make
* GNU Tar
* jsontool (`npm install -g jsontool`)

## Hints

* Use `ph-project-switch-buffer-other-project` command (by default bound
  to `S-F3`) to quickly switch to some opened file in another project.

* Add `.ph` files to `.gitignore`. I don't see a point of having them in
  git repos. `.ph` files are updated whenever you open/close a new file
  & there is no useful metadata in them.

* Make a symlink to `bin/ph-project-new` in some PATH directory if you'd
  like to make new projects (or upgrade existing) ones from the command
  line or shell scripts.

* `M-x describe-minor-mode RET ph RET` shows current ph keyboard
  bindings.

## Licence

MIT.
