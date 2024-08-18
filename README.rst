====================
My .doom.d directory
====================

This repository is the content of my ``$HOME/.doom`` directory.

It is intended to be portable to all of the machines on which I use Emacs.

Initial setup
=============

Copy aside the old ``.emacs.d`` directory, if necessary:

.. code:: shell

  $ mv ~/.emacs.d ~/.emacs.d.old

Then set up the Doom dependencies (since I'm on a Mac, I use homebrew, as discussed at
https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#with-homebrew):

.. code:: shell

  $ # required dependencies
  $ brew install git ripgrep
  $ # optional dependencies
  $ brew install coreutils fd
  $ # Installs clang
  $ xcode-select --install

Install an appropriate Emacs. The recommendation used to be emacs-plus, and
I've stayed with that (since emacs-plus@28, the flicker issue that is
mentioned in the Doom README, and which I'd definitely experienced, seems
to be fixed)

.. code:: shell

  $ brew tap d12frosted/emacs-plus
  $ brew install emacs-plus --with-native-comp --with-no-frame-refocus

(see the `emacs-plus README`_ for the meaning of the switches)

.. _`emacs-plus README`: https://github.com/d12frosted/homebrew-emacs-plus

And then, on an Intel Mac:

.. code:: shell

  $ ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app

or on an M1 Mac:

.. code:: shell

  $ ln -s /opt/homebrew/opt/emacs-plus@28/Emacs.app /Applications


**NOTE**

   As of Dec 2021 the recommendation (in the Doom `getting started, with
   homebrew`_ guide)  is for emacs-mac, but I can't get that to
   work with my use of emacsclient. This is a known problem - see

   * https://www.reddit.com/r/emacs/comments/rphhze/running_emacsclient_t_open_in_terminal_instead_of/
   * https://github.com/railwaycat/homebrew-emacsmacport/issues/52
   * https://ylluminarious.github.io/2019/05/23/how-to-fix-the-emacs-mac-port-for-multi-tty-access/
   * https://github.com/DarwinAwardWinner/mac-pseudo-daemon

   Unfortunately, the mac-pseudo-daemon package doesn't seem to help with
   use of emacsclient to create the initial GUI instance.

.. _`Getting started, with homebrew`: https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#with-homebrew

Once that is done, put this repository in place:

.. code:: shell

  $ cd ~
  $ git clone git@github.com:tibs/dotDoom.git .doom.d

(this is done now so that the next step won't overwrite it).

New (the Doom Emacs repository moved, and its main README now recommends
installing to `~/.config/emacs`):

        Then follow
        https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org#doom-emacs
        and install Doom Emacs itself:

        .. code:: shell

             git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
             ~/.config/emacs/bin/doom install

        Make sure that `~/.config/emacs/bin` is on the PATH

        See the rest of https://github.com/doomemacs/doomemacs for lots of other information.


Old:

        Then follow
        https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#doom-emacs
        and install Doom Emacs itself:

        .. code:: shell

          git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
          ~/.emacs.d/bin/doom install

        See the rest of https://github.com/hlissner/doom-emacs for lots of other information.


If Emacs won't start, you may need to reinstall:

.. code:: shell

  brew reinstall emacs-plus --with-native-comp --with-no-frame-refocus

Other stuff
===========

Also in this directory:

* history.rst_ which describes my initial setting up of Doom
* emacs-notes.rst_ which contains all the (unsorted!) Emacs notes I've been
  collecting over the last few years

And see https://github.com/tibs/dotEmacs which is/was my original repository
of shared Emacs stuff, specifically cotnaining my old ``.emacs.d/init.el``
file in ``dot.emacs.d/init.el``

.. _history.rst: history.rst
.. _emacs-notes.rst: emacs-notes.rst
