====================
My .doom.d directory
====================

This repository is the content of my ````$HOME/.doom`` directory.

It is intended to be portable to all of the machines on which I use Emacs.

Setting up the Doom dependencies on a Mac using homebrew (which is what I did)
is discussed at
https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#with-homebrew

.. code:: shell

  $ # required dependencies
  $ brew install git ripgrep
  $ # optional dependencies
  $ brew install coreutils fd
  $ # Installs clang
  $ xcode-select --install

(I went with emacs-plus as recommended):

.. code:: shell

  $ brew tap d12frosted/emacs-plus
  $ brew install emacs-plus
  $ ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app

Once that is done, put this repository in place:

.. code:: shell

  $ cd ~
  $ git clone git@github.com:tibs/dotDoom.git .doom.d

(this is so that the next step won't overwrite it).

Then follow
https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#doom-emacs
and install Doom Emacs itself:

.. code:: shell

  $ git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
  $ ~/.emacs.d/bin/doom install

See the rest of https://github.com/hlissner/doom-emacs for lots of other information.

-------------

Also see:

* history.rst_ which describes my initial setting up of Doom
* emacs-notes.rst_ which contains all the (unsorted!) Emacs notes I've been
  collecting over the last few years

Also, see https://github.com/tibs/dotEmacs which is/was my original repository
of shared Emacs stuff, specifically, my old ``.emacs.d/init.el`` file in
``dot.emacs.d/init.el``

.. _history.rst: history.rst
.. _emacs-notes.rst: emacs-notes.rst
