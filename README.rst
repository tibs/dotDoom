====================
My .doom.d directory
====================

This repository is the content of my ````$HOME/.doom`` directory.

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

Install an appropriate Emacs. As of Dec 2021 the recommendation appears to be
emacs-mac:

.. code:: shell

  $ brew tap railwaycat/emacsmacport
  $ brew install emacs-mac --with-modules
  $ ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications/Emacs.app

.. note:: I originally went with emacs-plus as that used to be recommended,
   but it seemed worth changing over as I was getting the flashing effects
   mentioned in the Doom readme.

   .. code:: shell

     $ brew tap d12frosted/emacs-plus
     $ brew install emacs-plus
     $ ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app

Once that is done, put this repository in place:

.. code:: shell

  $ cd ~
  $ git clone git@github.com:tibs/dotDoom.git .doom.d

(this is done now so that the next step won't overwrite it).

Then follow
https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#doom-emacs
and install Doom Emacs itself:

.. code:: shell

  $ git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
  $ ~/.emacs.d/bin/doom install

See the rest of https://github.com/hlissner/doom-emacs for lots of other information.

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
