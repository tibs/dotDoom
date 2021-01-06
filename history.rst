==============================
Doom Emacs - my original notes
==============================

My normal emacs setup (see https://github.com/tibs/dotEmacs) was:

* ~/<somewhere>/dotEmacs

  * emacs-notes.txt
  * doom.text - this document
  * dot.emacs.d

    * init.el - my normal emacs init file

* ~/.emacs.d

  * init.el@ -> ~/<somewhere>/dotEmacs/dot.emacs.d/init.el
  * and all the other traditional stuff that gets put into .emacs.d by emacs itself

----------

https://github.com/hlissner/doom-emacs

----------

Let's follow the instructions at
https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org

::

  $ brew install git ripgrep     # already installed, updates git
  $ brew install coreutils fd    # fd was already installed
  $ xcode-select --install       # already installed

Doom Emacs doesn't want the homebrew installed emacs I've been using, so let's
try using the emacs it recommends.

First uninstall the old emacs, which was the one from emacsformacosx.com,
originally installed using ``brew cask install emacs``::

  $ brew cask uninstall emacs

and the install `emacs-plus`_::

  $ brew tap d12frosted/emacs-plus
  $ brew install emacs-plus
  $ ln -s /usr/local/opt/emacs-plus@27/Emacs.app /Applications/Extras/

and then continue editing this document using my ``evil`` command line command
to verify that it works (!).

----------

For the moment, let's install Doom as an alternative to my existing emacs
setup. So I'll follow the instructions for using Chemacs on the Doom page.

As they suggest, do::

  $ git clone https://github.com/hlissner/doom-emacs ~/doom-emacs
  $ ~/doom-emacs/bin/doom install

Answer "y" to ``Generate an envvar file?`` (this creates a snapshot of my
shell environment, rather than the way that the other emacs does it by trying
to introspect it at startup using ``exec-path-from-shell``. See
``~/doom-emacs/bin/doom help env`` for more information...)

Answer "y" to ``Download and install all-the-icon's fonts?``.

It finishes by saying::

  Finished! Doom is ready to go!

  But before you doom yourself, here are some things you should know:

  1. Don't forget to run 'doom sync', then restart Emacs, after modifying
    ~/.doom.d/init.el or ~/.doom.d/packages.el.

    This command ensures needed packages are installed, orphaned packages are
    removed, and your autoloads/cache files are up to date. When in doubt, run
    'doom sync'!

  2. If something goes wrong, run `doom doctor`. It diagnoses common issues with
    your environment and setup, and may offer clues about what is wrong.

  3. Use 'doom upgrade' to update Doom. Doing it any other way will require
    additional steps. Run 'doom help upgrade' to understand those extra steps.

  4. Access Doom's documentation from within Emacs via 'SPC h d h' or 'C-h d h'
    (or 'M-x doom/help')

  Have fun!

Now go over to the chemacs site at https://github.com/plexus/chemacs and
follow its directions (since the directions on the Doom page claim they won't
work if I've got a ``~/.emacs.d`` directory, but the direction at the chemacs
site don't seem to mind)

Ah - the problem is presumably that Doom emacs wants to use ~/.emacs.d for its
own purposes, and in particular it wants its own ``init.el`` in that directory.

I *think* the solution is to move my current ~/.emacs.d into ~/.config/emacs,
and use chemacs to choose. ::

  mv ~/.emacs.d ~/.config/emacs

To check, once I've done that I can still run ``emacs`` at the command line,
and emacs starts up - but unforunately it's clearly not "evil", so it doesn't
see that directory by default.

OK - leave it there for the moment, and assume I'll put it back by hand if I
need to. Let's just get Doom going first.

---------------

So the next thing to do is::

  $ ~/doom-emacs/bin/doom install

It creates ~/.doom.d, sees that the envvar file already exists, and asks if it
should ``Download and install all-the-icon's fonts`` - let's say "y" - and we
get::

  Finished! Doom is ready to go!

  But before you doom yourself, here are some things you should know:

  1. Don't forget to run 'doom sync', then restart Emacs, after modifying
    ~/.doom.d/init.el or ~/.doom.d/packages.el.

    This command ensures needed packages are installed, orphaned packages are
    removed, and your autoloads/cache files are up to date. When in doubt, run
    'doom sync'!

  2. If something goes wrong, run `doom doctor`. It diagnoses common issues with
    your environment and setup, and may offer clues about what is wrong.

  3. Use 'doom upgrade' to update Doom. Doing it any other way will require
    additional steps. Run 'doom help upgrade' to understand those extra steps.

  4. Access Doom's documentation from within Emacs via 'SPC h d h' or 'C-h d h'
    (or 'M-x doom/help')

  Have fun!

again. Also::

  $ ls ~/doom-emacs/
  LICENSE          bin/             docs/            init.el          modules/
  README.md        core/            early-init.el    init.example.el  test/
  
  $ ls ~/.doom.d/
  config.el    init.el      packages.el

  $ ls ~/.emacs.d/
  auto-save-list/

Let's run::

  $ $ ~/doom-emacs/bin/doom doctor

to see what it says. ::

  The doctor will see you now...

  > Checking your Emacs version...
  > Checking for Emacs config conflicts...
  > Checking for great Emacs features...
  > Checking for private config conflicts...
  > Checking for stale elc files...
  > Checking Doom Emacs...
    ✓ Initialized Doom Emacs 2.0.9
    ✓ Detected 30 modules
    ✓ Detected 118 packages
    > Checking Doom core for irregularities...
      Found font material-design-icons.ttf
      Found font weathericons.ttf
      Found font octicons.ttf
      Found font fontawesome.ttf
      Found font file-icons.ttf
      Found font all-the-icons.ttf
    > Checking for stale elc files in your DOOMDIR...
    > Checking your enabled modules...
      > :lang sh
        ! Couldn't find shellcheck. Shell script linting will not work

  There are 1 warnings!

Fair enough. ::

  $ brew install shellcheck

and now "the doctor" is happy.

Now let's try::

  $ ~/doom-emacs/bin/doom run

but that seems to just run a vanilla emacs, not evil or doom like at all.

----------

OK. Given I've moved my ~/.emacs.d directory aside, let's do this differently.

::

  $ mv ~/.config/emacs ~/.emacs.d.original

(I can then always put it back "by hand" if needs be).

and uninstall Doom::

  $ rm -rf doom-emacs .doom.d .emacs.d

and now follow the "normal" instructions for installing it::

  $ git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
  $ ~/.emacs.d/bin/doom install

again, saying "y" to creating an envvar file at ``~/.emacs.d/.local/env`` and
to downloading the fonts. And it finishes as before.

**Now** running ``emacs`` (in another window) starts up Doom, and it does it
very quickly (and in dark mode, of course).

Files to configure appear to be (in ~/.doom.d/):

* config.el
* init.el
* packages.el

According to the "getting stated" guide:

  * init.el

    Where you’ll find your doom! block, which controls what Doom modules are
    enabled and in what order they will be loaded.
    
    This file is evaluated early when Emacs is starting up; before any other
    module has loaded. You generally shouldn’t add code to this file unless
    you’re targeting Doom’s CLI or something that needs to be configured very
    early in the startup process.

  * config.el

    Here is where 99.99% of your private configuration should go. Anything in
    here is evaluated after all other modules have loaded, when starting up
    Emacs.
  
  * packages.el
    
    Package management is done from this file; where you’ll declare what
    packages to install and where from.

 Edit the ``config.el`` file to set my name and emaill address, and to choose
 a different theme.

 https://github.com/hlissner/doom-emacs/tree/develop/modules/ui/doom#changing-theme
 indicates that the themes are from
 https://github.com/hlissner/emacs-doom-themes

 The default is doom-one - let's try doome-one-light to start with.

 OK. That will do for the moment, at least.

 Then think about what I really want from my "normal" init.el

 That's going to be a slow process, as I check each thing against the doom
 defaults - but there are a couple of obvious things to start with.

 (but it really is true that Doom loads *very* fast - it actually feels faster
 than using the emacs server!)

 OK. Still learning, but I think I've got the basics working...

 NB: ``SPC f r`` stopped working - it worked when I first installed doom, and
 something seems to have stopped it working. ::

   doom emacs "error in post-command hook ivy--queue-exhibit void-function case

.. _`emacs-plus`: https://github.com/d12frosted/homebrew-emacs-plus

And now at work
===============

Over Christmas break, I put all of this on github and tidied it up a bit.

And consequently I'm now using Doom Emacs at work as well.

I had two things to fix:

* ``Q q`` - I'd forgotten that this was my own kebinding, following vim, and
  it was basically just a matter of uncommenting the code in my ``config.el``
* ``open-logfile`` - one of the functions inside that was using ``case`` and
  for doom I need to use ``cl-case``

Also, it looks as if ``SPC f r`` is indeed now working with the latest
versions of everything, so that's nice.
