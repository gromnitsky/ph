# Emacs Project Helper

FIXME: a brave link to youtube

* For general design see `doc/structure.org`.
* For HOWTO see the screencast. (Sorry for ruglish & awful accent,
  though).
* For advices, critics, bugs et al, please fill Issues on Github.

## Why It's Cool

* Nothing to configure
* Support any types of projects & any files.
* Ido-style project files switching
* Any number of opened projects simultaneously
* Auto-remembering/-forgetting what file belongs to a project
* Open/close project files with 1 command.

FIXME: explain more

## Installation

`.emacs`:

    (add-to-list 'load-path "/directory/with/ph")

	(require 'ph)
    (ph-mode)

Optionally byte-compile it by typing

    % make compile

in the tarball directory.

## Quick Start

`M-x` ph-project-new

And choose some directory with your pet project files. (It's better to
close those files in emacs at first, but don't worry it's just for the
1st time & never again.)

`C-x f` some-file-in-the-chosen-directory
`C-x f` another-file-in-the-chosen-directory
`C-x f` and-another-file-in-the-chosen-directory

`M-x` ph-project-switch-buffer

Nice, right?

`M-x` ph-project-new

And choose some other directory with your other pet project files.

`M-x` ph-project-switch

Nice, right?

Then

`M-x` ph-project-close

Yay! Then switch to previous project:

`M-x` ph-project-switch

And

`M-x` ph-project-close

Finally, to blow your mind:

`M-x` ph-project-open

And choose .ph file location. OMG! It's so cool!

## Requirements

* git
* Linux/BSD
* Emacs 24.3
* GNU Make

Tested only under Fedora 17 and Emacs 24.3.50.1 (2013-02-08) from trunk.

Probably won't work under Windows at all. No idea about OSX.

## Licence

MIT.
