====================
Notes on Emacs stuff
====================

I introduced use of Emacs (and specifically XEMacs) at Laser-Scan in about
1988 (following use of EDT and TPU on VMS).

I moved (from XEMacs) to Vim when I started at SJ Consulting in 2004.

Then in 2019 I started using evil mode...

.. note:: **Tidy up this file(!)**

  Organise into:

  * things I must remember to do to reproduce my setup
  * useful links and information
  * things I want to fix / change
  * things I want to explore / learn more about
  * things that cannot be fixed - they're intrinsic to being in Emacs
  * things I decided *not* to do, and why

.. contents::


Reproducing my setup
====================

As of January 2019, the version of emacs provided by Mac OS is:

    GNU Emacs 22.1.1

which is a bit sad, as the current version seems to be 26.1!

https://www.emacswiki.org/emacs/EmacsForMacOS says that the recommended way to
install with homebrew is::

  brew cask install emacs

which installs a pre-built package from https://emacsformacosx.com/ (26.1.2
when I first did it), creating ``/Applications/Emacs.app/``::

  ==> Installing Cask emacs
  ==> Moving App 'Emacs.app' to '/Applications/Emacs.app'.
  ==> Linking Binary 'Emacs' to '/usr/local/bin/emacs'.
  ==> Linking Binary 'ebrowse' to '/usr/local/bin/ebrowse'.
  ==> Linking Binary 'emacsclient' to '/usr/local/bin/emacsclient'.
  ==> Linking Binary 'etags' to '/usr/local/bin/etags'.

And now I can start emacs, just as expected. Running ``emacs`` at the command
line gives me a GUI emacs, and ``emacs -nw`` gives me the terminal version.

----------------

I have set up an 'evil' command in fish, to run emacsclient. Note that I can
still pass ``-nw`` to the command for terminal mode.

.. note:: I could have put ``(server-start)`` in my Emacs initialisation file,
          but in practice emacsclient will start a new server for me if
          necessary (given the right switch). And with the current mechanism I
          can distinguish ``emacs`` (starts a new emacs instance) and ``evil``
          (uses emacsclient).

In my ~/config/fish/config.fish, instead of::

     set --export EDITOR (which vim)

I now do::

     set --export EDITOR 'emacsclient --alternate-editor="" -nw'
     set --export VISUAL 'emacsclient --alternate-editor="" --create-frame emacs'

(giving a name to the buffer in the second case stops it opening the
``*scratch*`` buffer, which makes more sense).

Trying Spacemacs or Doom Emacs
==============================

From the Spacemacs FAQ (http://spacemacs.org/doc/FAQ.html):

    How do I: Try Spacemacs without modifying my existing Emacs configuration?

    Emacs' ability to use any directory as the home for launching it allows us
    to try out Spacemacs (or any other Emacs configuration we desire) without
    having to go through the trouble of backing up our ~/.emacs.d directory
    and then cloning the new configuration. This can be achieved easily using
    the following steps::

        mkdir ~/spacemacs
        git clone git@github.com:syl20bnr/spacemacs.git ~/spacemacs/.emacs.d
        HOME=~/spacemacs emacs

    If you're on Fish shell, you will need to modify the last command to::

        env HOME=$HOME/spacemacs emacs

Spacemacs with everything installed has a reputation for getting slow.


Interesting Doom links:

* https://github.com/hlissner/doom-emacs

  * https://github.com/hlissner/doom-emacs/wiki
  * https://github.com/hlissner/doom-emacs/wiki/Getting-Started
  * https://github.com/hlissner/doom-emacs/wiki/FAQ

* https://medium.com/urbint-engineering/emacs-doom-for-newbies-1f8038604e3b
* https://dsdshcym.github.io/blog/2018/01/22/compare-doom-emacs-spacemacs-vanilla-emacs/
* https://www.rousette.org.uk/archives/back-to-doom-emacs/
* https://noelwelsh.com/posts/2019-01-10-doom-emacs.html
* https://medium.com/@aria_39488/getting-started-with-doom-emacs-a-great-transition-from-vim-to-emacs-9bab8e0d8458

Doom still sounds worth playing with.

Useful links and information
============================

...


Keybindings
-----------
Emacs keystrokes I still need to remember

* C-x k -- to kill a non-evil-ised buffer
* C-g -- for general giving up (the evil documentation acknowledges this, I think)
* C-c C-c -- useful in, for instance, exiting wdired mode
* C-h k <key> to find out what function a key is bound to, and all the other
  C-h bindings.

``<CTRL>-Z`` toggle Emacs "state" (Emacs already has things it calls "modes",
so Evil calls its vim modes "states").

https://support.apple.com/en-us/HT201236 lists the standard Mac keyboard
shortcuts using:

* Command (or Cmd) ⌘
* Option (or Alt) ⌥
* Control (or Ctrl) ⌃
* Caps Lock ⇪
* Shift ⇧
* Fn

It also says "On keyboards made for Windows PCs, use the Alt key instead of
Option, and the Windows logo key instead of Command."

I *think* the only ones I really care about are CMD-C and CMD-V, with some
slightly less caring about CMD-X and CMD-A.

However, it is possible I might come to care about some of the window
manipulation commands (for instance), so perhaps using CMD as META is not such
a great idea...

Caps-lock *is* sitting there...

On the other hand: Aquamacs "just" defines some special keybindings for the keys
that it wants to be available as M-<x>. So in its
``site-lisp/macosx/emulate-mac-keyboard.el`` it does (amongst other things)::

    (setq emulate-mac-keyboard-mode-maps
     `(
       ...
       (us . (    ("\M-3" . "£")
                  ("\M-@" . "€")
      ("\M-6" . "§")))
       (brtish . (("\M-3" . "#")
                  ("\M-2" . "€")
      ("\M-6" . "§")))))

although I'm not terribly convinced by that "§" as it has its own key, paired with "±",
on my keyboard.

There's then code to *use* that. I think that, on the whole, this actually seems like
a less invasive solution - it leaves Meta on the Opt (Alt) key, where Emacs "normally"
puts it.

What would those M-3 (and so on) normally do, if anything?


Things I want to fix/change
===========================

Things to check
---------------

* Emacs in the terminal - does it work properly? Specifically, do the
  following work as I expect?

    * Cut and paste
    * Meta key

* Emacs lisp:

    * How do I stop it telling me I've laid my emacs lisp file out wrong?
    * How should I lay my emacs lisp file out?
    * How do I stop it complaining about "free variables" that are actually
      perfectly good emacs values?

      The official answer seems to be to ``(defvar <name>)`` which asserts it
      is dynamically bound.

     * Maybe set line width for emacs lisp (? to 120) as well.

* Maybe look at saving sessions, perhaps based on the name of the
  computer. Does it work when I use emacsclient?

* Do I want pipenv integration? and maybe whether there is similar for poetry,
  just in case. Although simple virtualenv support *might* be enough.

  - for instance, https://github.com/pwalsh/pipenv.el

Things I want to do the same
----------------------------
What things do I set up in vim, and want the same (or similar) in Emacs?

* Default shiftwidth 2 or 4, depending
* Default soft tabs where I want them
* Tab stop 8
* Sharing of my emacs setup between different machines (via github, presumably)
* Does dedent (rub out of indentation) work by the indentation or the single spaces?
  In Vim it was definitely the former.
* Maybe make tabs visible
* Maybe show indentation steps visibly
* In Vim I used font Monaco:h12 in the GUI
* Think about colours

  For Vim I have something like::

    " Set nice colors
    " background for normal text is light grey
    " Text below the last line is darker grey
    " Cursor is green, Cyan when ":lmap" mappings are active
    " Constants are not underlined but have a slightly lighter background
    highlight Normal guibg=grey90
    highlight Cursor guibg=Green guifg=NONE
    highlight lCursor guibg=Cyan guifg=NONE
    highlight NonText guibg=grey80
    highlight Constant gui=NONE guibg=grey95
    highlight Special gui=NONE guibg=grey95
    highlight  StatusLine term=bold,reverse cterm=NONE ctermfg=White ctermbg=Black gui=NONE guifg=White guibg=Black
    highlight  StatusLineNC term=bold,reverse cterm=NONE ctermfg=White ctermbg=Black gui=NONE guifg=White guibg=DarkSlateGray

* Can I get evil to honour the ``.. vim: .. :`` line in a file, instead of
  needing the emacs equivalent? (``:help vim:`` shows it to be called a
  "modeline" in vim. There is

    https://github.com/cinsk/emacs-vim-modeline

  as referenced by

    https://emacs.stackexchange.com/questions/46826/emulate-vim-modeline-in-evil-mode

  (from December 2018) - no idea if this works, and not sure how to hand
  things like ``filetype=rst``). It certainly appears to support *some* of the
  things I want.

* Where is my ``:view`` and ``:sview``?

* When I hit tab at the end of a line, I want it to add spaces (for instance
  when wanting to tab over to add a comment). Certainly in Python.

* In Vim Python mode, ``[[`` and ``]]`` move over classes (i.e., to start/end
  of class as appropriate). My fingers expect that, but it doesn't work the
  same way in evil. Maybe the definition of whatever ``[[`` moves over is
  different between the two editing environments.

Open questions
--------------

* Am I sure that I'm (a) not leaving trailing whitespace behind (e.g., in
  reStructuredText) and (b) that if it's there I see it (ditto)?

* Can I have multiple emacs servers, so I can (for instance) have source code
  files in a separate context than personal documentation files? Or is there
  another way of doing that, some sort of session management?

* Can I add syntax highlighting for YAML (``*.yml``) and JSON (``.json``)?

* Can I add syntax checking for YAML (``*.yml``) and JSON (``.json``)?

Actual problems
---------------

* (Probably) FIXED

  ``:%s`` complains "% s is undefined". This appears to be cause by my setting
  up ``%\``  to match what I used to do in vim. The simplest solution seems to
  be to change to ``^\`` instead, in both Emacs and Vim - that's just one key
  over, and I don't *think* that ``^`` has a special meaning in ``:``.

* Is there a way to stop changing to another (Mac) window when I exit evil and
  that was the last evil frame in the first window? It's somewhat
  disconcerting to have my focussed window change like that.

  Is this because of emacsclient (or does it matter how I created the deleted
  frame - I suspect not)?

  Can I program "close frame, drop focus" instead of "close frame, transfer
  focus"?

* Quite often, when I move focus to an Emacs window, by clicking on it with
  the track pad, I end up with a region selected. This *may* be because I'm
  accidentally double-tapping (right mouse) instead of single-tapping (left
  mouse). See
  https://www.gnu.org/software/emacs/manual/html_node/emacs/Mouse-Commands.html,
  where it says::

    Clicking with the right mouse button, mouse-3, runs the command
    mouse-save-then-kill. This performs several actions depending on where you
    click and the status of the region:

    * If no region is active, clicking mouse-3 activates the region, placing
      the mark where point was and point at the clicked position.

  That does sound plausibly like my problem, and why I get "random"
  selections. Of course, if I don't notice this and carry on editing, I can
  end up doing things to that region, when I didn't want to.

  I have a feeling that what I really want to do is to greatly
  simplify/restrict what the "mouse" operations actually are - after all,
  since I'm coming (back) from Vim, I'm not *expecting* sophisticated mouse
  operations anyway.

  https://www.gnu.org/software/emacs/manual/html_node/emacs/Mouse-Buttons.html
  talks about how to (re)define what the mouse operations should be.

  What *do* I use the trackpad to do (in an editor context)?

  - focus on the window (tap with one finger)
  - move point (the cursor) to a new position
  - drag to select a region

  In general I also:

  - tap with two fingers to get right mouse (but I *expect* that to give me a
    popup menu, not to select).
  - drag with two fingers, to scroll - this doesn't seem to be a problem in an
    emacs context
  - drag with three fingers, to resize - similarly, not a problem in an emacs
    context

  and I may or may not expect a double tap with one finger to select a "word"
  (it does that in the terminal, and seems to behave the same in emacs, I just
  can't remember how much I use it!).

  https://stackoverflow.com/questions/4906534/disable-mouse-clicks-in-emacs
  which mentions https://github.com/purcell/disable-mouse (disable the mouse
  in emacs), and various other places also seem to suggest this is a good
  solution.

  https://emacs.stackexchange.com/questions/14246/is-it-possible-to-disable-the-support-for-the-mouse-in-gui/22539
  sounds like it is relevant in general, and *it* has a pointer to
  `disable-mouse`_ at the end.

  ...So I've added the use of disable-mouse to my init.el, hopefully with
  the right sort of settings. It *does* mean that clicking on a window does
  not move point to the mouse location (as, I suppose, advertised). I'm not
  sure if that's a problem, or whether I want to allow that.

  That also means that I can't choose a different "split" in a frame using the
  mouse - just the frame as a whole. Hmm.

  ...and I can't select a range to do CMD-C on it. Hmm. I liked being able to
  do that.

  But scrolling still works (which I think is useful).

  So there may be a bit more customisation to be done.

  **NB:** See the note on use of a mouse (or not) in flyspell elsewhere in
  this document.

* When I highlight some text in emacs, if I do CMD-V *too fast* then it
  doesn't get into the (system) clipboard. I should probably investigate
  this...

* It doesn't quite feel as if <make visual selecion> and then ``P`` works
  quite the same way - specifically, if I select several lines and then do
  ``P`` with my cursor inside a line, sometimes it puts the region into the
  line, rather than after it. It *might* be that I've inadvertently selected
  visual mode before typing ``P``?

  (I *think* this may be because when I click on a different window, or a
  different place in a window, then some sort of visual selection often gets
  made - either a single character, or even from point to end of line.)

* In Python mode (at least), paragraph fill doesn't seem to work. Noted for
  text in docstrings. And I think also for long comments - not wrapping to a
  new comment line starting with ``#``. (Command used: ``Qq`` in normal mode)

* Hmm. Trailing whitespace (especially when it's all that is on a line)
  doesn't necessarily show up in red - I see this in Python, for instance.

* Why *does* the frame content "white out" when I resize it? It's very
  irritating, and I don't know of any other program that does it. It's also
  hard to google for...

* If I set up emacsclient as my EDITOR, then I seem to have problems when I
  (for instance) do ``git commit`` - it seems to be waiting for the editor to
  appear. At the moment I can just continue to use vim in this context, but I
  should probably work out what I want.

* If I run ``evil -nw`` I still (perhaps sometimes?) seem to get some sort of
  menu bar along the top.

* In Vim I can do ⌘+ and ⌘- (CMD-<plus> and CMD-<equal>, in fact) to resize
  the font, just as I can elsewhere. In Emacs the eqiuivalent (per buffer
  only) is C-x C-+ and C-x C--. See elsewhere in this text for other
  approaches, particuarly the presentation-text-mode package..

Things to learn more about
==========================
Things which look as if they might be interesting.

...

Things that cannot be fixed
===========================
These are things that I might want otherwise, but they're too closely entwined
with Emacs to be changeable.

...

Things I decided *not* to do
============================
Things I thought about, but decided not to do.

...

---------------------------------------------------------------------------


Useful links
============

Emacs on Mac

* https://www.emacswiki.org/emacs/EmacsForMacOS - where I (re) started
* 2013 https://korewanetadesu.com/emacs-on-os-x.html - Configuring Emacs on Mac OS X
* https://www.reddit.com/r/emacs/comments/92ynak/macos_and_emacs_setup/ -
  MacOS and Emacs Setup

Specific builds of interest (other than "normal" Emacs)

* https://emacsformacosx.com/ - GNU Emacs for Mac OS X - the Emacs Wiki says
  "These builds are based on the development version of GNU Emacs and do not
  contain any additional packages or patches. Popular Mac keyboard shortcuts
  are available though (e.g. Command-O for opening a file); these are mapped
  to the Super modifier (i.e., the Apple/Command key functions as Super)."
  The site splashes "Pure Emacs! No Extras! No Nonsense!". Unfortunately,
  there doesn't seem to be much information on its website on what makes it
  a distinct distribtion.

  Regardless, this is the version of emacs that homebrew instals when I do

    brew cask install emacs

  Its git repository is at http://git.savannah.gnu.org/cgit/emacs.git/

* https://bitbucket.org/mituharu/emacs-mac/ - the Emacs Wiki says "based on
  the latest stable release of GNU Emacs (26.1 as of 2018-06-14) and claims to
  incorporate most of the features of Carbon Emacs and the Carbon+AppKit port
  from Emacs 22.3. It has improved C-g support, an emulation of ‘select’ that
  doesn’t require periodic polling, full screen support, subpixel font
  rendering, and smooth (pixel) scrolling. See the readme for more details.
  Available via Homebrew and MacPorts (as emacs-mac-app), as discussed below,
  or prebuilt binaries." or use::

    brew tap railwaycat/emacsmacport
    brew install emacs-mac

* https://tuhdo.github.io/ - Emacs mini-manual

**On the whole** I feel more comfortable using "plain" Emacs, which builds
fine for Cocoa/Mac rather than relying on dependent distributions - for
instance, I'd been using Aquamacs, but that seems to be rather inactive, and
is definitely trailing well behind the current version of Emacs.

Hmm. Some of the advice at
https://www.reddit.com/r/emacs/comments/dxxwdr/configuring_emacs_from_scratch/f7xcfwy/
sounds sensible. Part of the discussion at
https://www.reddit.com/r/emacs/comments/dxxwdr/configuring_emacs_from_scratch/
reacting to an article on medium.

General

* 2018 https://huytd.github.io/emacs-from-scratch.html - Emacs from scratch
* 2018 https://github.com/emacs-tw/awesome-emacs - A community driven list of
  useful Emacs packages, libraries and others.
* 2018 https://news.ycombinator.com/item?id=16551796 - Hacker News thread for
  "Ditching a bunch of stuff and moving to Emacs and org-mode" which is at
  https://changelog.complete.org/archives/9861-emacs-1-ditching-a-bunch-of-stuff-and-moving-to-emacs-and-org-mode
* 2019 https://github.com/hlissner/doom-emacs - An Emacs configuration for the
  stubborn martian vimmer. Meant to be slimmer and close to vanilla emacs than
  spacemacs.
* 2019 https://github.com/MatthewZMD/.emacs.d - M-EMACS, my custom GNU Emacs
  distribution. Uses ``use-package``, has lots of explanation. Note its
  ``rename-file-and-buffer``, which it says comes from
  http://steve.yegge.googlepages.com/my-dot-emacs-file, and which I probably
  want to borrow as well. Also includes use of
  https://github.com/DarthFennec/highlight-indent-guides
  and
  https://github.com/skuro/plantuml-mode
  and setup of LSP and DAP modes.

* 2019 https://github.com/caisah/emacs.dz - A list of people with nice emacs
  config files. Note that the table indicates if a setup is evil or not, and
  whether it uses use-package. Looks worth working through.

* 2014 https://github.com/bbatsov/emacs-lisp-style-guide/ - The Emacs Lisp
  Style Guide
* 2017 https://github.com/chrisdone/elisp-guide - Emacs Lisp Guide. Meant to
  be just enough practical knowledge to allow one to do useful stuff.
* 2016
  https://sam217pa.github.io/2016/09/23/keybindings-strategies-in-emacs/ - Key
  binding strategies in Emacs (nb: the writer is an Evil user) and the Redit
  comments on it at https://www.reddit.com/r/emacs/comments/545v9p/keybindings_strategies_in_emacs/
* 2018 https://dsdshcym.github.io/blog/2018/01/22/compare-doom-emacs-spacemacs-vanilla-emacs/
  - the author decides to maintain a fork of Doom Emacs, instead of using
  Spacemacs or managing their own vim bindings on top of vanilla emacs.
* 2018 https://dougie.io/emacs/indentation/ - The Ultimate Guide To
  Indentation in Emacs (Tabs and Spaces)
* 2013 https://nullprogram.com/blog/2013/02/06/ - How to Make an Emacs Minor
  Mode
* 2019 https://jonathanabennett.github.io/blog/2019/06/20/python-and-emacs-pt.-1/
  - first of a series as he figures out how to setup emacs for his python use.
  Using pyenv, pipenv, elpy, company and jedi.
* 2019 https://realpython.com/emacs-the-best-python-editor/ - as it says


People's emacs init files

* 2019 http://nhoffman.github.io/.emacs.d/ - init.el for Noah Hoffman

  This appears to have some rather useful Mac OS advice (including iTerm2
  settings) near the start...

* 2019 https://www.john2x.com/emacs.html - John's Emacs Config
  - includes LSP setup, and is evil
* 2018 https://m00natic.github.io/emacs/emacs-wiki.html - Andrey's Opinionated
  Emacs Guide
* 2018 http://aaronbedra.com/emacs.d/ - Aaron Bedra's Emacs 26 Configuration
* 2019 https://github.com/zzamboni/dot-emacs/blob/master/init.org - a literate version using org-mode
  - the blog post is at https://zzamboni.org/post/my-emacs-configuration-with-commentary/ - there's
  quite a lot on org-mode there as well.
* Even more ambitious: http://doc.rix.si/cce/cce.html - Emacs as a Complete
  Computing Environment (the author, on Linux, even uses Emacs to run the
  windowing system, less possible on the Mac as is pointed out by Irreal in
  2018, at https://irreal.org/blog/?p=7270). The cce pages still seem to be
  being updated in 2019. Much of it appears to be done as org-mode files that
  tangle out to the actual emacs lisp.
* https://github.com/mclear-tools/dotemacs/blob/master/config.org is another
  literate configuration using org-mode. Lots of stuff there, and also some
  pointers to simpler setups that might be worth studying. And they're an evil
  user.
* https://github.com/Abuelodelanada/pepe-emacs-config is an example
  configuration with lots of pictures showing it in action. It includes
  directory tree (neotree) and "IDE"-like stuff (ECB). Not evil.
* https://github.com/NateEag/.emacs.d. Uses evil-mode. The README has the
  following interesting (quotable) text:

        Like all art forms, programming has technique.

        A musician's technique is how he makes the instrument produce sound.

        A painter's technique is how she puts paint on the canvas.

        A programmer's technique is how he gives the computer instructions.

        Despite the grumbling from the graphical language crowd, most
        programming comes down to entering, reading, and changing plain text
        in files.

        Thus, a programmer should manipulate text fluidly and effortlessly,
        the way a pianist plays arpeggios or a painter wields a brush.

        Changing editors for each language complicates technique. Eclipse for
        Java, PyCharm for Python, Sublime for JavaScript... The keystrokes for
        editing a program are different in each of these, and over a lifetime
        adds cognitive burden.

        Instead of changing editors for each language, a programmer's editor
        should adapt itself to each language, so that the technique of
        programming remains unchanged.

        In the same way, the programmer should not adapt herself to the
        editor - the editor should adapt to her.

        For these purposes, Emacs reigns supreme.

        It has been honed over decades to a razor-sharp edge, it runs almost
        everywhere, and it can be rewritten without restarting it.

* https://github.com/jcs090218/jcs-emacs-init - lots of stuff, including links
  to articles on speeding up Emacs:

  * https://anuragpeshne.github.io/essays/emacsSpeed.html
  * https://emacs.stackexchange.com/questions/2286/what-can-i-do-to-speed-up-my-start-up

* https://github.com/purcell/emacs.d is by the author of `disable-mouse`_ which
  I reference elsewhere. May or may not be of direct interest: it does set up
  support for various interesting programming languages, but it is not evil.

.. _`disable-mouse`: https://github.com/purcell/disable-mouse

* https://github.com/ideasman42/emacs-for-vimmers - an introductory Emacs
  configuration, especially aimed at vim-users. "This is meant to be a
  minimal, somewhat opinionated Emacs configuration intended to for Vim users
  who prefer to build their own init file instead of more heavy weight
  solutions. ... This is the configuration I wish I’d had starting out."

  Tries to explain what each item in the config is for, and also *why*.

  Also, an example of using ``use-package``.

  **RECOMMENDED** for looking at in the future.

* https://systemreboot.net/dot-emacs - evil

* 2020 https://github.com/grettke/lolsmacs - The Law Of Least Surprise Lattice
  For Emacs.

   | ;; Intuitive impersonal settings complying with the Law Of Least Surprise
   | ;; meant for inclusion with any initialization file especially useful to
   | ;; first-time Emacs users or experienced Emacs users looking for focused
   | ;; high-value content to copy.

Perhaps only tangentially interesting:
https://lispcookbook.github.io/cl-cookbook/emacs-ide.html,
"The Common Lisp Cookbook – Using Emacs as an IDE"

https://github.com/zenspider/enhanced-ruby-mode - a different Ruby mode than
the one that comes with emacs. Is it better?

https://github.com/rubocop-hq/rubocop-emacs - run rubocop_, although
apparently flycheck should actually already support this.

.. _rubocop: https://github.com/rubocop-hq/rubocop

Integration of Emacs and Pry_ (a runtime developer console and IRB alternative
with powerful introspection capabilities):
https://dev.to/thiagoa/ruby-and-emacs-tip-advanced-pry-integration-33bk

.. _Pry: https://github.com/pry/pry

https://github.com/glyph/python-docstring-mode - minor mode for Python
docstrings. Knows reStructuredText and epydoc (memories!).

.. note:: I'm using Sphinx for my logbook notes, and in particular I have
   occasion to use the ``:doc:`` and ``:ref:`` roles. This causes flycheck to
   get unhappy, and it says ``Unknown interpreted text role "doc"`` (or
   ``"ref"``). There must be a way to sort that out...

   I don't think sphinx-mode (https://github.com/Fuco1/sphinx-mode) helps
   directly, but it does say it defines support for ``:ref:``, so I may be
   able to copy that.

**HERE, NOTICE THIS!**

.. note:: I think I particularly want to study the core emacs setup in
          http://doc.rix.si/cce/cce-emacs-core.html

More:

* 2019 https://www.sandeepnambiar.com/my-minimal-emacs-setup/
* 2019 https://github.com/Atman50/emacs-config - A literate emacs
  configuration for C#, python, ivy, yasnippet, ...
* 2019 https://github.com/dakra/dmacs/blob/master/init.org - a rather large
  literate emacs configuration
* 2019 http://budevg.github.io/posts/tools/2019/05/08/dev-environment.html
  - development environment with Emacs, XMonad and NixOS - probably more
  interesting as an example of all three together, because the Emacs setup is
  very heavily split into parts that get "built" together, and there's a lot
  of it.
* https://gitlab.com/ambrevar/emacs-fish-completion - and how to use fish in
  ``M-x shell``
* https://github.com/galdor/rfc-mode - for reading RFCs
* https://github.com/alezost/shift-number.el - an alternative implementation
  of incrementing/decrementing the (next) number.
* https://github.com/ambirdsall/moon-phase/blob/master/moon-phase - emacs lisp
  version of moon phase calculation. https://github.com/ambirdsall/moon-phase.
  Use a shell script to run emacs to calculate the moon phase and put it in
  your prompt!

http://www.emacs-bootstrap.com/ aims to generate Emacs init.el files for
various programming languages. Probably worth playing with.

Evil mode

* https://www.emacswiki.org/emacs/Evil - where I (re) started
* 2018 https://github.com/noctuid/evil-guide - Draft of a guide for using emacs with evil
* https://brainlessdeveloper.com/2017/12/27/making-emacs-work-like-my-vim-setup/
  - Making Emacs work like my Neovim setup
* http://evgeni.io/posts/quick-start-evil-mode/ - Quick Start emacs with evil
* https://github.com/emacs-evil/evil-collection - A set of (sets of)
  keybindings for evil-mode
* https://www.reddit.com/r/emacs/comments/7e9wcy/when_coming_from_vim_should_i_use_evilmode_or/
  - When coming from vim, should I use evil-mode OR learn emacs key bindings?
* 2018o https://www.linode.com/docs/tools-reference/tools/emacs-evil-mode/ - How to
  navigate Emacs usign Evil Mode
* 2016 https://blog.aaronbieber.com/2016/01/23/living-in-evil.html - Living in
  Evil - this manages its packages differently (using ``use-package``) but has
  some really useful advice on some general management of how evil interacts
  with the rest of Emacs. He had an earlier post, in 2015,
  https://blog.aaronbieber.com/2015/05/24/from-vim-to-emacs-in-fourteen-days.html
* 2014 https://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
  - From Vim to Emacs+Evil chaotic migragion guide
* 2005 http://ergoemacs.org/emacs/keyboard_shortcuts.html - Emacs: How to define keys
* 2010/2018 http://ergoemacs.org/emacs/emacs_hyper_super_keys.html - Emacs:
  How to Bind Super Hyper Keys, with a very nice picture of the relevant
  keys(!).  The related page http://xahlee.info/kbd/banish_key_chords.html
  also talks about sticky keys.
* 2014 https://blog.jakuba.net/2014/06/23/Evil-Mode-How-I-Switched-From-VIM-to-Emacs/
  - has a few interesting keybindings
* https://www.reddit.com/r/emacs/comments/b216t3/what_do_you_recommend_to_a_standard_emacs_user_to/
  - What do you recommend to a <standard> Emacs user to get into Evil?

Also:

* 2016 http://cachestocaches.com/2016/12/vim-within-emacs-anecdotal-guide/ -
  Vim within Emacs: An anecdotal guide
* 2018: https://dev.to/huytd/emacs-from-scratch-1cg6 - Emacs from scratch
  (building an evil configuration from nothing) and a GIST for their
  actual configuration https://gist.github.com/huytd/6b785bdaeb595401d69adc7797e5c22c
  Note that they're quite a nice example of using use-package in a clear
  manner.
* 2019 https://www.reddit.com/r/emacs/comments/cq5esr/do_you_ensure_t/
  - a discussion of whether to use ``:ensure t`` in use-package. Minor but
  useful.

Even more also:

* https://github.com/jmorag/kakoune.el - a very simple simulation of the
  kakoune editor inside of emacs.

reStructuredText support

* https://www.emacswiki.org/emacs/reStructuredText
* https://github.com/akheron/emacs-config/blob/master/lib/rst-mode.el

and also

* https://github.com/Fuco1/sphinx-mode - a minor mode for Sphinx

Other stuff

* 2019 https://prelude.emacsredux.com/en/latest/ - start of a manual for the
  Emacs Prelude distribution.
* https://github.com/jorgenschaefer/emacs-buttercup - BDD for Emacs Lisp
* 2018 https://www.iro.umontreal.ca/~monnier/hopl-4-emacs-lisp.pdf - paper on
  the evolution of Emacs lisp.
* 2010/2018 https://www.masteringemacs.org/article/mastering-key-bindings-emacs
  - Mastering Key Bindings in Emacs
* https://alphapapa.github.io/emacs-package-dev-handbook/ -- lots of varied
  notes and tips, looks worth revisiting as I learn more
* and https://www.emacswiki.org/emacs/ElispCookbook
* 2014 https://yoo2080.wordpress.com/2014/07/04/it-is-not-hard-to-read-lisp-code/
* http://pages.sachachua.com/emacs-notes/how-to-read-emacs-lisp.html - Read
  Lisp, Tweak Emacs: How to read Emacs Lisp so that you can customize Emacs
* http://pages.sachachua.com/.emacs.d/Sacha.html - their heavily documented
  init file.
* 2019 http://mbork.pl/2019-04-15_How_to_make_a_menu_in_Emacs - How to make a
  menu in Emacs. But not drop-down menus: "there is always the “official”
  drop-down menu, bound to F10 by default – but I’m less interested in
  drop-down menus here."
* https://github.com/kai2nenobu/guide-key - allow a popup showing key sequence
  completions.
* https://github.com/proofit404/blacken - black mode for emacs (see black at
  https://github.com/python/black/):

    Blacken uses black to format a Python buffer.  It can be called
    explicitly on a certain buffer, but more conveniently, a minor-mode
    'blacken-mode' is provided that turns on automatically running
    black on a buffer before saving.

* 2018 https://www.masteringemacs.org/article/running-shells-in-emacs-overview
* 2019 https://jonathanabennett.github.io/blog/2019/06/05/file-management-in-emacs-with-dired-mode/
  Note that this has a good description of using use-package to lazily load
  evil-collection for dired-mode.

* https://github.com/purcell/default-text-scale - easily adjust the font size
  in all Emacs frames, by rebinding C-x C=+ and C-x C--

  Note that C-x C-+ and C-x C-- will adjust the font size of the current
  buffer, so if that's all I want, I don't need to do anything else.

  https://github.com/zonuexe/emacs-presentation-mode then provides a
  presentation mode, entered (and exited) with M-x presentation-mode, in which
  those key strokes affect all buffers. I prefer that approach, but
  unfortunately this package is not available in the standard Emacs package
  repositories, so I'd have to clone it by hand. So this is something to think
  about another time.

* https://github.com/fasheng/dired-toggle - show dired as a sidebar
* https://github.com/Wilfred/deadgrep - use ripgrep from Emacs
* https://github.com/NixOS/nix-mode - an Emacs major mode for editing Nix
  expressions
* https://github.com/spotify/dockerfile-mode - Dockerfile mode

and lots of interesting suggestions at
https://stackoverflow.com/questions/8483182/evil-mode-best-practice
- note the suggestions at https://stackoverflow.com/a/21518286 for possible
alternative usages.

odd packages:

* https://github.com/linktohack/evil-commentary - commenting out lines
* https://github.com/davidshepherd7/aggressive-fill-paragraph-mode - does this
  help with my wish for paragraph fill to be more like Vim?
* https://github.com/Fuco1/dired-hacks - things to make dired-mode even more
  useful
* https://github.com/Silex/docker.el - docker support in emacs

    Supported commands

    - docker container: attach, cp, diff, inspect, kill, logs, pause, rename, restart, rm, start, stop, unpause
    - docker image: inspect, pull, push, rm, run, tag
    - docker network: rm
    - docker volume: rm
    - docker-machine: create, env, restart, rm, start, stop
    - docker-compose: build, config, create, down, exec, logs, pull, push, remove, restart, run, start, stop, up

    You can also enter dired or open a file inside a container or volume.

The (perhaps inevitable) https://github.com/remacs/remacs - rewriting Emacs
(at least the C parts?) in Rust.

* https://github.com/purcell/page-break-lines - make ^L look like a horizontal
  rule.
* https://github.com/joostkremers/visual-fill-column - Instead of wrapping
  lines at the window edge, which is the standard behaviour of
  visual-line-mode, it wraps lines at fill-column
* https://github.com/dajva/rg.el - run ripgrep in emacs
* https://github.com/redguardtoo/evil-matchit - Press “%” to jump between
  matched tags in Emacs. For example, in HTML “<div>” and “</div>” are a pair
  of tags. Equivalent of matchit.vim
* https://github.com/syl20bnr/evil-tutor - vimtutor adapted for Evil mode.

* https://github.com/rakete/bird-mode - bird's eye mode for emacs buffers -
  not sure what this is meant to do?

* 2019 https://yiufung.net/post/pure-emacs-lisp-init-skeleton/ - Pure
  Emacs-Lisp Init Skeleton
* https://github.com/a13/emacs.d - another example emacs setup, with useful
  commentary.

* https://github.com/polymode/polymode - a framework for multiple major
  modes (MMM) inside a single Emacs buffer.

* https://beorg.app/orgmode/letsgetgoing/ - Let's get going with Org mode (a
  gentle introduction).

* https://github.com/Alexander-Miller/treemacs/ - a tree layout file explorer
  for Emacs.
* https://github.com/jaypei/emacs-neotree - another

* https://github.com/pashky/restclient.el - HTTP REST client for emacs

.. _`projects and perspectives`:

Organising windows/frames by their use, so I don't end up with all my buffers
in one "bucket":

* https://github.com/bmag/emacs-purpose - organise windows by "purpose". See
  https://github.com/bmag/emacs-purpose/wiki for documentation.
* https://github.com/nex3/perspective-el - provides multiple workspaces (or
  "perspectives") for each Emacs frame. This makes it easy to work on many
  separate projects without getting lost in all the buffers.

  https://www.reddit.com/r/emacs/comments/bx7m7a/how_to_deal_with_a_lot_of_buffers_in_emacs/
  asks "How to deal with a lot of buffers in emacs?" and the response is:

    You want perspective-el combined with projectile.

    The former lets you create named groups, each of which contains a distinct
    list of buffers and a distinct window arrangement, and switch between
    them. So you could make a perspective for one project, and another for
    another project, yet a third for life organization, and a fourth for Emacs
    hacking. They will not interfere with each other.

    The latter adds a slew of specialized commands which work on projects,
    where a project is loosely defined as directory with a special marker
    file. It lets you rapidly access files located under that directory,
    switch to buffers relating only to files in that project, search through
    the project, and so on. Projectile has solid integration with both Helm
    and Ivy.

    I work on many different projects at the same time, and consider both of
    these packages essential to a smooth Emacs workflow. Otherwise everything
    becomes much too disorganized and difficult to find.

  See also `2020-02-06`_

Hmm. Themes:

* https://emacsthemes.com/

but also Xah Lee's http://ergoemacs.org/emacs/emacs_customize_default_window_size.html
(which also addresses background colour) and http://ergoemacs.org/emacs/emacs_playing_with_color_theme.html

Lexical binding:

* 2013 https://yoo2080.wordpress.com/2013/09/11/emacs-lisp-lexical-binding-gotchas-and-related-best-practices/
* https://www.emacswiki.org/emacs/DynamicBindingVsLexicalBinding

Maybe learn how to use the (provided) whitespace package - although its defaults appear to be quite ugly.

* 2017 https://nullprogram.com/blog/2017/08/22/ - Vim vs. Emacs: the Working
  Directory
* 2019 https://idiocy.org/emacs-fonts-and-fontsets.html - maybe not something
  I actually need, but an interesting part of the Emacs infrastructure.
* https://github.com/rolandwalker/unicode-fonts - Configure Unicode fonts for
  Emacs (actual advice on what fonts to download and how to set them up).

Emacs modeline

* https://seagle0128.github.io/doom-modeline/ - evil compatible
* https://www.emacswiki.org/emacs/ModeLine
* https://www.gnu.org/software/emacs/manual/html_node/emacs/Mode-Line.html
* 2008/2017 http://ergoemacs.org/emacs/modernization_mode_line.html - some
  proposals
* 2017 http://www.holgerschurig.de/en/emacs-tayloring-the-built-in-mode-line/
* https://github.com/dbordak/telephone-line - quite vibrant
* Versions of powerline:

  * https://github.com/Dewdrops/powerline
  * https://www.emacswiki.org/emacs/PowerLine - some varied links, the above
    is the last

Modal "text object" based editing in Emacs *without* going to Vim/Evil:
https://github.com/clemera/objed. """A global minor-mode to navigate and edit
text objects. Objed also enables modal editing and composition of commands. It
combines ideas of versor-mode and other Editors like Vim or Kakoune and tries
to align them with regular Emacs conventions."""

* https://github.com/jtmoulia/elisp-koans - TDD learning of Emacs Lisp. Still
  under construction.
* https://www.reddit.com/r/emacs/comments/cduuxb/opinions_on_railwaycats_emacsmacport/
  - "Opinions on Railwaycat's Emacs-MacPort"
* https://ylluminarious.github.io/2019/05/23/emacs-mac-port-introduction/
  then discusses that same port in some detail.
* https://www.reddit.com/r/emacs/comments/cdm5oo/which_of_these_are_mutually_exclusive_or/
  - "Which of these are mutually exclusive or redundant: Semantic, Cscope, GNU
  Global, ycmd, Language Server Protocol, Irony-Mode and RTags?"

  Also see the ongoing discussion on #help-emacs in slack at work.

* I may already have a link for this, but https://github.com/remacs/remacs,
  the rewrite of the Emacs core in Rust. It sounds like some people are using
  it day-to-day (presumably it "should always work" as things are either in C
  or Rust?)
* One of those *other* editors in development that always sounds interesting:
  https://xi-editor.io/
* https://melpa.org/#/poetry - an interface to poetry
  (https://poetry.eustace.io/) - actually hosted at https://github.com/galaunay/poetry.el

* https://www.reddit.com/r/emacs/comments/cih8lm/vacation_reading_material_about_emacs/
  although I'm not sure there's anything (or anything new that I didn't
  already know about) here.

* 2018 https://www.spacjer.com/blog/2018/03/02/why-is-my-emacs-slow/ - how to
  profile emacs

* https://github.com/dp12/parrot - polly wants to rotate a word (party parrot)

* https://github.com/skuro/plantuml-mode/ - with support for previewing

* https://github.com/edkolev/evil-goggles/ - display a visual hint on evil
  edit operations

* https://github.com/luxbock/evil-cleverparens - modal-editing optimized for
  editing Lisp. Not edited in the last couple of years. Mentions:

  * https://github.com/abo-abo/lispy - rich in features, not vim/evil like.
  * https://github.com/roman/evil-paredit - errors out when the user gets
    parens wrong. evil-cleverparens started as a fork of it.
  * https://github.com/syl20bnr/evil-lisp-state - adds an extra evil state for
    editing lisp.
  * https://github.com/expez/evil-smartparens - the package they'd have
    contributed to, instead of starting their own, if they'd known about it.

  and entirely separately (and probably *way* too far for me!),
  https://github.com/countvajhula/symex.el, an evil way to edit Lisp symbolic
  expressions ("symexes") as trees in Emacs. Currently built on top of
  paredit, lispy, and evil-cleverparens.

* https://www.emacswiki.org/emacs/RegularExpression - Emacs regular
  expressions. Necessary for when I'm wanting to do less simple searches using
  ``/``, or regexp ``:%s`` replacements.

* https://github.com/jacktasia/dumb-jump - """an Emacs "jump to definition"
  package for 40+ languages""" - automated use of ``rg`` to try to jump to the
  definition of things.
* https://www.reddit.com/r/emacs/comments/d45d14/getting_rid_of_line_breaks/
  - how to "undo" line breaks in a paragraph, e.g., before pasting into some
  other program.

  - ``M-x delete-indentation``
  - https://github.com/purcell/unfill/ - the inverse of fill-paragraph and
    fill-region

* https://github.com/emacscollective/no-littering - Help keeping ~/.emacs.d
  clean. See also:

  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Auto_002dSaving.html
  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html

* https://github.com/stanaka/dash-at-point - run dash (documentation thing) on
  the word at point. NB: I've paid for Dash, as tibs@tonyibbs.co.uk - search
  my emails for that email address for the link to the license file.

Highlighting:

* https://github.com/emacsmirror/auto-dim-other-buffers
* https://github.com/gonewest818/dimmer.el also dims other buffers
* https://github.com/tobias/downplay-mode can be used to focus attention on
  sections of a buffer during a demo or presentation.

More other stuff:

* https://gitlab.com/kisaragi-hiu/didyoumean.el (it is on melpa) - might be
  interesting?
* https://github.com/pashky/restclient.el - REST client in Emacs
* https://github.com/yuya373/emacs-slack - slack frontend in Emacs
* https://github.com/firstrow/fat-free-evil - goals are Vim emulation without
  evil, Minimal setup, Fast startup time ~1 second, As close to vanilla emacs
  as possible.
* https://github.com/wasamasa/nov.el - epub reading mode (now in my init.el,
  may or may not be working well - how do I tell if my emacs is built with
  libxml2? - it says "rendering will fail" if I don't have it).
* https://github.com/tripleee/my-site-start - a simple Elisp library to help
  you keep your init.el clean and modular (by auto-loading elisp files from
  a pre-specified directory structure)
* https://github.com/technomancy/better-defaults - "Better Defaults for
  Emacs". Doesn't say it's on MELPA, but see
  https://github.com/melpa/melpa/blob/master/recipes/better-defaults, which
  basically does::

    (better-defaults :fetcher github :repo "technomancy/better-defaults")

  which is interesting!

More links on learning [emacs] lisp:

* https://www.gnu.org/software/emacs/manual/pdf/eintr.pdf - Introduction to
  Programming in Emacs Lisp (which is also available via ``C-h m lisp intro <RET>``)
* https://emacs.stackexchange.com/questions/47318/how-can-i-start-learning-emacs-lisp/47331
  maybe starting at
  https://emacs.stackexchange.com/questions/47318/how-can-i-start-learning-emacs-lisp/47331#47331
  which recommends the first 3 chapters of https://github.com/norvig/paip-lisp
  (Peter Norvig's Paradigms of Artificial Intelligence Programming)
  and/or
  https://emacs.stackexchange.com/questions/47318/how-can-i-start-learning-emacs-lisp/47321#47321
  which recommends starting at https://www.emacswiki.org/emacs/LearnEmacsLisp
* Practical Common Lisp
* http://weitz.de/cl-recipes/ - Common Lisp Recipes
* https://exercism.io/ has an Emacs Lisp module (as well as lots of others, of course)

More other stuff:

* https://github.com/AdamNiederer/cov/ - displaying coverage data on your code

https://medium.com/@suvratapte/configuring-emacs-from-scratch-use-package-c30382297877
is part 3 of a series, but discusses the organisation of an init.el, and how
to use ``use-package`` (and bootstrap that use).

* https://blog.davep.dev/nuke-buffersel-tidy-up-open-buffers-in-emacs-ck45lup0n01riu4s1exohaxrf
  - Tidy up open buffers in Emacs - https://github.com/davep/nuke-buffers.el
  Might be useful, but does what the original author wants, so very much a
  "read the source code and alter to fit".

https://github.com/brandelune/nipel - a new introduction to Emacs Lisp. Still
in heavy alpha, apparently, as of December 2019. Appears to expect you to
clone the repository and then edit the file
``new_introduction_to_programming_in_emacs_lisp.org``

Note:

  you can switch between emacs and evil state using ``C-z``

but also see the thread at
https://www.reddit.com/r/emacs/comments/e8knmb/emacs_tip_configure_a_key_binding_to_switch/
for more ideas.

* http://salvi.chaosnet.org/texts/emacs-challenge - Emacs Challenge - try to
  solve the emacs puzzles therein
* 2019 https://stegosaurusdormant.com/emacs-ripgrep/ - Blazing-fast
  jump-to-grep in Emacs using ripgrep
* https://github.com/benma/visual-regexp.el/ - A regexp/replace command for
  Emacs with interactive visual feedback

  https://github.com/benma/visual-regexp-steroids.el/ is the same thing, but
  using "modern regexp engines" instead of Emacs regexps. "you can optionally
  use the better regexp syntax to power isearch-forward-regexp and
  isearch-backward-regexp". Uses Python and https://github.com/joddie/pcre2el
  (to convert syntaxes).

* https://metaredux.com/posts/2019/12/07/dead-simple-emacs-screencasts.html -
  how to show the key (combination) being typed, as it is typed, shortly
  followed by
  https://metaredux.com/posts/2019/12/08/recording-screencasts-with-emacs.html
  - how to actually record a screencast using emacs (not necessarily a good
  way to *actually* record a screencast).

* https://metaredux.com/posts/2019/12/09/dealing-with-expired-elpa-gpg-keys.html


Catalina problems with Emacs
============================

https://spin.atomicobject.com/2019/12/12/fixing-emacs-macos-catalina/ - Fixing
Emacs After an Upgrade. How to work around Emacs no longer being able to
access *all* user folders - e.g., the ``Downloads`` folder. Caused by the User
Data Protection system on Catalina, and the way the Emacs app works.


Installation
============


NB: I already had the file ``~/.emacs.d/init.el`` created, with the following
content::

  (require 'package)

  (setq package-archives
    '(("gnu"         . "http://elpa.gnu.org/packages/")
      ("melpa"       . "http://melpa.milkbox.net/packages/")))

  (package-initialize)

  (when (not (package-installed-p 'evil))
    (package-refresh-contents)
    (package-install 'evil))

  (evil-mode)

  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(package-selected-packages (quote (evil))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )

and I was able to exit emacs using ``:q!``

Useful notes
============

Maybe look at:

From the evil github repositories at https://github.com/emacs-evil:

* https://github.com/emacs-evil/evil-surround - evil-surround (I didn't use
  surround in vim, but probably should learn it)
* evil-magit (make magit more evil)

  On magit itself:

  * https://emacsair.me/2017/09/01/the-magical-git-interface - Magit, the
    magical Git interface
  * https://emacsair.me/2017/09/01/magit-walk-through/ - A walk through the
    Magit interface

From elsewhere:

* https://github.com/redguardtoo/evil-nerd-commenter
* https://github.com/dgutov/diff-hl - highlight uncommitted changes
* https://github.com/Lindydancer/lisp-extra-font-lock

Helm

* https://tuhdo.github.io/helm-intro.html - intro to helm
* https://github.com/emacs-helm/helm
* https://emacs-helm.github.io/helm/ - documentation
* See the section on helm in http://cachestocaches.com/2016/12/vim-within-emacs-anecdotal-guide/
* https://noctuid.github.io/blog/2015/02/03/a-more-evil-helm/
* https://github.com/emacs-evil/evil-collection/ (evil collection) has helm
  bindings: https://github.com/emacs-evil/evil-collection/blob/master/evil-collection-helm.el

and
https://www.reddit.com/r/emacs/comments/bsc8pc/why_did_you_stop_using_helm/
explains why some people stopped using Helm (and moved to Ivy instead)

Ivy:

* https://github.com/abo-abo/swiper (actually Ivy, Counsel and Swiper)
* https://oremacs.com/swiper/

If I do decide to use org-mode, evil-collectons links to

* https://github.com/GuiltyDolphin/org-evil
* https://github.com/Somelauw/evil-org-mode

Built-in things:

* http://emacs-fu.blogspot.com/2011/03/ielm-repl-for-emacs.html - elisp repl

* https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
* 2015 https://ebzzry.io/en/emacs-tips-2/ (various things)
* https://www.emacswiki.org/emacs/SessionManagement

Someone on the internet said::

  To test different Emacs configs simply set EMACS_USER_DIRECTORY to the
  desired path (environment variable).

-- https://www.reddit.com/r/emacs/comments/bmufza/finding_emacs_distributions_and_trying_them_out/en029rm/

---------


* python-mode is part of the standard emacs setup. The prelude_ distribution sets up
  Python with that mode and flycheck_ for on-the fly syntax checking, so that seems
  the sensible thing for me to integrate...

.. _flycheck: https://github.com/flycheck/flycheck
.. _prelude: https://github.com/bbatsov/prelude

Things I still need to do:

- set up more Python stuff - autocomplete would be nice!

- consider automatically switching flyspell on, at least in some modes.
- consider setting flyspell to use a British dictionary, and then make sure
  I've got one
- make flyspell easier to use without a mouse.

  flyspell wants to use middle-click, mouse-2. That's a pain when I'm using a
  magic track pad - I've got mouse-1 as tap, and mouse-3 as
  tap-with-two-fingers, but no mouse-2.

  One suggestion is to use a tool like BetterTouchTool (https://www.boastr.net/)
  and define a custom gesture. But that's overly complicated because there
  *isn't* an obvious unambiguous gesture I want to use.

  Another suggestion, at
  https://superuser.com/questions/364575/rebinding-s-mouse-1-to-mouse-2-in-emacs-on-os-x/547646
  is to remap some mousey configurations - not sure if that's great either,
  but it may be worth trying. Apparently one should be able to do::

    (define-key key-translation-map (kbd "<C-mouse-1>") (kbd "<mouse-2>"))

  to map control-left-click as middle-click. Although my first attempt at
  using that doesn't seem to work...

  However, the alternate (and more specific)::

    (global-set-key [M-down-mouse-1] 'flyspell-correct-word)

  does work. I *suspect* that something is grabbing CTRL mouse-1 before it
  gets to "my" code, but not META mouse-1.

  Finally, https://groups.google.com/forum/#!topic/gnu.emacs.help/VrN_uLaWS1E
  has some other suggestions for how to do keyboard-based solutions.

  .. note:: There are presumably other packages that are also going to want to
            use mouse-2, so this may be something that it would be worth
            finding a comfortable solution for.

- figure out how to show tabs and indentation lines (maybe)

  ...perhaps https://github.com/DarthFennec/highlight-indent-guides

- look at ``M-x regexp-builder``

evil-command-window-current-buffer - the buffer from which the command line
window was called.

History: https://irreal.org/blog/?p=8067 - More on Multics Emacs. Links to
documents on how Emacs was developed on Multics.

Ruby in Emacs
=============

* https://wikemacs.org/wiki/Ruby
* https://www.emacswiki.org/emacs/RubyOnRails

Articles:

* https://stackoverflow.com/questions/2429373/tips-and-tricks-for-using-emacs-to-develop-a-ruby-on-rails-app
  but it's from 2010
* 2019 https://dev.to/thiagoa/ruby-and-emacs-tip-advanced-pry-integration-33bk
  (using Pry in emacs)
* 2013 https://crypt.codemancers.com/posts/2013-09-26-setting-up-emacs-as-development-environment-on-osx/
* 2016 https://worace.works/2016/06/07/getting-started-with-emacs-for-ruby/

Modes:

* https://github.com/zenspider/enhanced-ruby-mode (last edited this
  year, 2019. This is actually a fork of the original, but that hasn't been
  updated since 2012.)
* https://github.com/remvee/emacs-rails (last edited 2013)
* https://github.com/asok/projectile-rails (last edited this year, 2019)

Also:

* https://github.com/porras/evil-ruby-text-objects - add some Ruby text
  objects and keybindings to Evil

Language Server Protocol (LSP)
==============================

https://langserver.org/ "A community-driven source of knowledge for Language
Server Protocol implementations"

The two main Emacs clients are

* https://github.com/emacs-lsp/lsp-mode

  * https://github.com/emacs-lsp/lsp-mode/#configuration - setting up
    lsp-mode, Language Server Protocol support. One of several solutions, I
    think.
  * https://github.com/emacs-lsp/lsp-docker/ - Docker image + scripts for
    running lsp-mode in docker environment.
  * https://github.com/sebastiencs/company-box/ - A company front-end with
    icons
  * https://github.com/emacs-lsp/dap-mode - Emacs Debug Adapter Protocol
  * https://www.reddit.com/r/emacs/comments/e6lzci/announcement_lspmode_62_released/
    sounds good, and here is the referenced
    https://www.reddit.com/r/emacs/comments/ahzrg0/announcement_lspmode_60_released/

* https://github.com/joaotavora/eglot - Emacs Polyglot: an Emacs LSP client
  that stays out of your way (a more minimal, intended to be simpler to use,
  LSP implementation than lsp-mode)

For the moment, I've gone for eglot (with company) because I could get it
to work (!). However, it proably doesn't support dap-mode, if I ever want
that.

There has been much discussion on which is "better", mostly inconclusive:

* https://www.reddit.com/r/emacs/comments/c90vge/lspmode_or_elgot/ (2019)
* https://github.com/joaotavora/eglot/issues/180 (2018) suggests that the
  eglot comparison to lsp-mode are outdated, and some to-and-fro after
  that.
* https://www.reddit.com/r/emacs/comments/do2z6y/i_am_moving_from_lspmode_to_eglot/
  (2019) says it is in reaction to that dialogue.

For Ruby, there is also
https://github.com/guskovd/emacs-solargraph, which seems to give direct
access to solargraph (https://github.com/castwide/solargraph).

What I'm using at work
----------------------

**tldr;** I’ve ended up installing company and eglot at work, because I could
 get that to work.

* https://github.com/emacs-lsp/lsp-mode
* https://github.com/joaotavora/eglot

It’s not obvious which I would want, but lsp-mode is the more traditional, in
some sense, and eglot is meant to (maybe) be cleaner and simpler to set up,
but perhaps not supporting as much. Although I do like the eglot readme.

Do ``M-X package-refresh-contents`` and:

* ``M-X package-install RET lsp-mode``
* ``M-x package-install RET eglot``
* ``M-x package-install RET company``

Do ``gem install solargraph`` to install the Ruby LSP server.

Do ``python3 -m pip install 'python-language-server[all]'`` to install the
Python LSP server.

Basically, it looks as if lsp-mode needs a lot of detailed setup, and it (a)
didn’t seem to do anything when editing a Python file, and (b) (maybe related)
didn’t seem to make it obvious if it was using an LSP server, even when
company-mode was enabled.

However, minimal setup of eglot (with ``company-mode`` enabled) does seem to
give me at least minimal completion, and it does show when an LSP server is
running (and, I think, what “project” it is using).

So let’s go with that.

Doom Emacs
==========
https://github.com/hlissner/doom-emacs
and https://github.com/hlissner/doom-emacs/wiki/Getting-Started
and https://github.com/hlissner/doom-emacs/wiki/Customization

* 2017 https://medium.com/urbint-engineering/emacs-doom-for-newbies-1f8038604e3b
* 2019 https://www.rousette.org.uk/archives/back-to-doom-emacs/

According to doom-emacs/modules/lang/ruby/packages.el, Doom Emacs
provides/uses enhaced-ruby-mode (I assume the version from
https://github.com/zenspider/enhanced-ruby-mode)

I've been investigating this as an alternative to my own home grown approach,
particularly for having programming environments already set up.

Things to follow up on:

* It looks like it uses ``exec-path-from-shell`` (guessing from having seen
  problems described at http://spacemacs.org/doc/FAQ.html issue 1.21) and that
  doesn't play well with ``fish``. So look into that.
  I'd rather not have to set ``exec-path`` myself.

* A directory browser down the side might be nice - do I just need to add a
  package?


From my fish command ``doom.fish``::

    function doom --description "Run Doom Emacs, from its github repository"
        # Assumes that I have done:
        #
        #  $ mkdir ~/odds
        #  $ cd ~/odds
        #  $ git clone git@github.com:hlissner/doom-emacs.git
        #  $ cd doom-emacs
        #  $ bin/doom/quickstart
        #
        # Note that the ``git clone`` *should* take it from the "develop" branch.
        #
        # Before you doom yourself, there are a few things you should know:

        # The quickstart finishes with the following information:
        #
        #   1. Whenever you edit your doom! block in ~/.doom.d/init.el or modify your
        #      modules, run:
        #
        #        bin/doom refresh
        #
        #      This will ensure all needed packages are installed, all orphaned packages are
        #      removed, and your autoloads files are up to date. This is important! If you
        #      forget to do this you will get errors!
        #
        #   2. If something inexplicably goes wrong, try `bin/doom doctor`
        #
        #      This will diagnose common issues with your environment and setup, and may
        #      give you clues about what is wrong.
        #
        #   3. Use `bin/doom upgrade` to update Doom. Doing it any other way may require
        #      additional work. When in doubt, run `bin/doom refresh`.
        #
        #   4. Check out `bin/doom help` to see what else `bin/doom` can do (and it is
        #      recommended you add ~/.emacs.d/bin to your PATH).
        #
        #   5. You can find Doom's documentation via `M-x doom/help` or `SPC h D`.

        # NB Needed to apply a temporary fix to allow downloading all the packages, following
        #   https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
        # so I added the line:
        #
        #  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
        #
        # to ~/odds/doom-emacs/init.el

        # NOTE that this fish command run Doom Emacs in the foreground.

        ~/odds/doom-emacs/bin/doom run
    end

And this post
https://www.reddit.com/r/emacs/comments/cfag4z/emacsp0rn_emacs_with_a_slick_ui_link_in_comments/
seems to have a variety of useful hints on things in it, including use of
treemacs. Links within:

* the author's actual emacs configuration https://github.com/ianpan870102/.use-package.emacs.d
* a "generalised" emacs distro based on the same https://github.com/ianpan870102/.emacs.d

https://github.com/alphapapa/alpha-org/blob/master/emacs-sandbox.sh - a shell
script to make it easy to run Emacs in a sandbox. Originally found at
https://www.reddit.com/r/emacs/comments/e4pcts/emacssandboxsh_run_emacs_in_a_clean_sandbox/,
and clearly useful if I want to play with different Emacs implementations/variants.

https://manuel-uberti.github.io//emacs/2019/12/01/el-patch/ is a story of how
the author did some stuff to stop psession_ from cluttering up their emacs
startup with warning messages.

.. _psession: https://github.com/thierryvolpiatto/psession

Evangelical article: https://ambrevar.xyz/emacs-everywhere/index.html -
running "everything" in emacs. Author's homepage, with links, at
https://ambrevar.xyz/, and "power apps" at https://ambrevar.xyz/power-apps/.
They especially like exwm_, a window manager *inside* emacs - all your X
windows are emacs buffers...

.. _exwm: https://github.com/ch11ng/exwm

https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
- Code folding with outline-minor-mode

https://gitlab.com/howardabrams/emacs-piper - fascinating - creates an
interactive user interface to dealing with unstructured textual data similar
with how we transform data through pipes in the shell.

Keeping notes
=============
So I'm currently keeping one file per day.

I've written a function (``open-logfile``) to create a new logfile for
"today" and add an appropriate heading - for instance::

    ======================
    Logbook for 2019-07-25
    ======================

    Thursday 25th July 2019

as I'm currently experimenting with one logbook per day.

If it's one logbook per week, then I would want to create a file for this
week, and add an overall heading for the week, and then a heading for today.

It would probably also be useful to have a command to go to (open in a buffer)
the logbook for a particular date, with some helpers for choosing yesterday, a
day of (the last) week, and maybe also tomorrow (so maybe an interactive
function that takes arguments like "yesterday", "today", tomorrow", "monday",
and so on, with two or three letter abbreviations also working).

One file per day is quite nice in many ways, but can be a bit finicky (so many
files), and will lead to work being spread over several files. And a file that
just says "Today was a holiday" feels wasteful.

One file per week leads to bigger files, and when work continues over several
days, the day headings feel somewhat intrusive.

Also, what to call the files? At the moment, I've got::

  ~/Documents/notes/logbook-YYYYMMDD.txt

and perhaps it would be better to use::

  ~/Documents/notes/logbook/YYYYMMDD.txt

On completion frameworks
========================
https://www.reddit.com/r/emacs/comments/ch0sp7/a_few_novice_questions_about_emacs_completion/

  """A lot of emacs commands boil down to "select an option from a big list."
  This includes package installation, simple M-x, switching to a buffer,
  opening a file, etc.

  The standard way of handling this is TAB completion inside Emacs. Helm
  instead uses an incremental search, so you type and things relating to what
  you type pop up. You can then select those items specifically and do
  different things (for example, you could use Helm to select a file and then
  dired inside the directory). It's really something I would recommend trying:
  some people don't like it, but I can barely go back to vanilla Emacs at this
  point. It's so helpful both for discovery (if you want to see all of the
  org-mode commands with the word list in them, you can do M-x org [SPC] list
  and they'll pop up) and for not wasting time on things like file
  selection."""

and:

  """ivy for a list to popup when you want to complete something.

  Helm for when you need a spreadsheet to popup to complete something.

  ivy is minimal, requiring another package counsel to operate on lists.

  helm is a bit of an abomination (the good kind). You may not see or use most
  of what it can do.

  Both do not handle completion for code unless setup to do so, but Company,
  or autocomplete are good dedicated packages."""

and:

  """Its a way to get what you want faster out of a large set of items. By
  default helm works in a window so it offers a larger view area, and ivy is
  one that works in the minibuffer (the bottom area of emacs) which offers a
  smaller view on a list of items. As with other areas of emacs many other
  completion systems exists as well though. Off the top of my head ido,
  snails, and raven for instance. So imo none are intrinsically better despite
  the loud voices on either side but offer different philosophies or designs
  perhaps. Best thing to do is to merely try them out and see what you
  like. From my own experience even the builtin completion is fine for most
  things. Its quick and simple. I use the builtin packages as well as ivy and
  helm for different things as I don't necessarily see them in competition but
  rather have different strengths in certain situations. Which is one of the
  funnest aspects of emacs: use it as it suits you! Use any and all packages
  as you see fit."""

* https://emacs-helm.github.io/helm/ - Helm
* https://github.com/abo-abo/swiper - Ivy, Counsel and Swiper (and the manual
  is at https://oremacs.com/swiper/)
* http://company-mode.github.io/ - Company == "complete anything".

https://www.reddit.com/r/emacs/comments/6x7ph2/is_company_different_from_helm_and_ivy/
says "Company-mode is a package for in-buffer code completion, while Helm/Ivy
are general narrowing-completion frameworks. Essentially, any time that you
have to make a choice from a list of candidates, Helm/Ivy will turn that
action into a narrowing completion list. This can be things like M-x,
find-file or switch-buffer."

Also see https://www.reddit.com/r/emacs/comments/6xc0im/ivy_counsel_swiper_company_helm_smex_and_evil/

And also there is https://github.com/abo-abo/hydra "make Emacs bindings that
stick around " or "tie related commands into a family of short bindings with a
common prefix - a Hydra."

On making Emacs "normal"
========================
https://www.reddit.com/r/emacs/comments/ch80re/mode_that_makes_emacs_behave_like_a_regular_text/

  """M-x cua-mode -- been built-in since v22.1"""

  """Besides cua-mode, I would add: delete-selection-mode. Having it off is
  something strange if you are not used to Emacs."""

Packaging and use-package
=========================

Should I be using ``use-package``? Almost certainly (or *perhaps* one of the
"more modern" alternatives).

Regardless, my init.el is now using it, in at least a basic way.

I *still* need to make it automatically load all the packages needed when I
first move my init.el in-place on a new computer (or, contrariwise, when I
update my init.el).

* use-package: https://github.com/jwiegley/use-package
* MELPA: https://melpa.org/#/getting-started
* The straight_ README has a comparison with other package managers:
  https://github.com/raxod502/straight.el/blob/develop/README.md#comparison-to-other-package-managers

  Note that straight and use-package are orthogonal.

  Hmm - I think I like the look of straight, but it may be best to get started
  just using the (default) package.el and (re)investigate this later on - I
  don't think straight gives me any advantages whilst I'm just wanting stable
  versions of packages without any frills.

  Found via https://www.reddit.com/r/emacs/comments/bwbo9n/interesting_emacs_packages_leafel_and_featherel/

  And also https://manuel-uberti.github.io//emacs/2019/11/02/thirty-straight-days/
  on strategies for updating packages with straight.el

* Another approach (perhaps complementary?) is to use `elpa-mirror`_
  - see https://www.reddit.com/r/emacs/comments/dynln5/advice_request_managing_melpaelpa_package/

  Irritatingly, using it requires downloading it or cloning its github
  repository, rather than getting it from melpa or equivalent.

* https://github.com/conao3/leaf.el is an alternative to use-package (it
  claims to benefit from being newer)
* https://github.com/conao3/feather.el is a new (parallel) package
  manager.

.. _straight: https://github.com/raxod502/straight.el
.. _`elpa-mirror`: https://github.com/redguardtoo/elpa-mirror

Examples of use:

* 2018: https://dev.to/huytd/emacs-from-scratch-1cg6 - Emacs from scratch
  (building an evil configuration from nothing) and a GIST for their
  actual configuration https://gist.github.com/huytd/6b785bdaeb595401d69adc7797e5c22c
  They're quite a nice example of using use-package in a clear manner.

* 2019 https://jonathanabennett.github.io/blog/2019/06/05/file-management-in-emacs-with-dired-mode/
  has a good description of using use-package to lazily load evil-collection for dired-mode.

Other links:

* 2015 http://cachestocaches.com/2015/8/getting-started-use-package/
* 2018 https://www.masteringemacs.org/article/spotlight-use-package-a-declarative-configuration-tool
* 2019 https://dev.to/deciduously/how-i-emacs-and-so-can-you-packages-m9p -
  includes use of use-package
* 2018 https://hiepph.github.io/post/2018-11-07-use-package/ - a couple of
  nice examples of usage

and other references elsewhere in this document.

Not Emacs
=========

Window management on a Mac
--------------------------

Or, attempts to make desktops/screens work sensibly, in a world where I unplug
my laptop from its external screens (yes, two of them).

* http://www.hammerspoon.org/ - all the automation. Press a key to perform an action.
* http://gridsutra.com/ - a bit short on documentation, likes videos for docs.
* https://manytricks.com/moom/ - $10 - try this 2nd if LayAuto doesn't seem adequate.
* https://lightpillar.com/mosaic.html - £9.99/£24.99
* https://layautoapp.com/ - $9.99 (free for 14 days) - tried, but didn't seem
  worth it

Neat apps
---------

World time buddy
~~~~~~~~~~~~~~~~
https://www.worldtimebuddy.com/ - there's also an app for iOS and Android.
And I've paid a yearly subscription, using email address tibs@tonyibbs.co.uk

Saved view:
https://www.worldtimebuddy.com/?pl=1&lid=5375480,2653941,1566083,1850147&h=2653941

Next browser - not there yet, but...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
https://next.atlas.engineer/ - Next browser,
https://github.com/atlas-engineer/next, "Next is a keyboard-oriented,
extensible web-browser designed for power users. The application has familiar
key-bindings (Emacs, VI), is fully configurable and extensible in Lisp, and
has powerful features for productive professionals." Unfortunately, at the
moment it looks like I'd need to build it from source, which might be a bit of
a pain.

Auto-keyboard detection
~~~~~~~~~~~~~~~~~~~~~~~
https://github.com/jeantil/autokbisw which Michael points out should help with
our having US keyboards on our work laptops, and UK keyboards on our desks
(and maybe me with other keyboards as well).

...and I've now been using this for a while, and it appears to work very well.

Very definitely not emacs! (it's Ruby)
--------------------------------------
In the threads at https://bugs.ruby-lang.org/issues/15799#note-46, matz
(Yukihiro Matsumoto) says:

    Unlike JavaScript and Python (Lisp-1 like languages), Ruby is a Lisp-2
    like language, in which methods and variable have separated namespaces. In
    Lisp-1 like languages, ``f1 = function; f1()`` calls function (single
    namespace).

https://github.com/sandro/specjour - distributed rspec/cucumber test runs

EDT
===
Yes, EDT!

* https://sourceforge.net/projects/edt-text-editor/
* http://edt-text-editor.sourceforge.net/

and http://texteditors.org/cgi-bin/wiki.pl?DecFamily is an interesting
resource, although it's links off-site don't seem particularly reliable.

It's instructive that there don't seem to be any easily findable ports of TPU...

Dump new unsorted links here
============================

Starting at the beginning of 2020, I'm aiming to dump new stuff here (at the
end of the file) for sorting out later (hah!).

2020-01-02
----------

* https://irreal.org/blog/?p=8557 - Why (or Why Not) Switch to Emacs

  Note for the "editors from the 1970s" talk. Links to a 1 hour video, but has
  a useful summary of some of the key points.

* https://jacmoes.wordpress.com/2019/09/24/creative-writing-with-emacs/ - a
  nice walk through of what Emacs is about and the basics of how to use it.
  Suitable as an introduction. The section on `inserting special characters`_
  is instructive.

.. _`inserting special characters`:
   https://jacmoes.wordpress.com/2019/09/24/creative-writing-with-emacs/#Emacs_Misc_Special_Characters

 * As pointed out in https://github.com/MatthewZMD/.emacs.d (M-EMACS, a
   customized full-featured GNU Emacs configuration ) the "Why Emacs?" section
   at https://github.com/remacs/remacs is lovely.

* https://github.com/ianpan870102/yay-evil-emacs - "lightweight literate Emacs
  config with even better "better defaults". Shipped with a custom theme!"

* 2016 https://sam217pa.github.io/2016/08/30/how-to-make-your-own-spacemacs/ -
  a little old, but some possibly useful examples of using use-package.

2020-01-07
----------

https://github.com/alhassy/emacs.d - another literate configuration, which is
meant to be easy to navigate. It looks like a very good document - it starts
with "Why Emacs?", for instance - and it "weaves" the document to allow
presentation of code fragments in a natural order *for the document*.

(It's looking pretty good as a document to read, by the way.)

* http://www.kotaweaver.com/blog/emacs-python-lsp/ - Setting up Emacs Python
  LSP with pyenv and stuff. Uses use-package, and lsp-mode (which I had
  trouble with).

* https://tkf.github.io/emacs-request/index.html - HTTP requests in Emacs

* https://randomgeekery.org/2020/01/01/quick-zoom-text-in-emacs/ - **TL;DR**:
  Use ``C-x C-=``, ``C-x C--``, and ``C-x C-0`` to scale text on the fly in
  your Emacs buffer.

* http://www.sbcl.org/index.html - Steel Bank Common Lisp - not Emacs, but one
  of the premier free lisps. Just released 2.0. And::

    $ brew info sbcl
    sbcl: stable 2.0.0 (bottled)
    Steel Bank Common Lisp system
    http://www.sbcl.org/
    Not installed
    From: https://github.com/Homebrew/homebrew-core/blob/master/Formula/sbcl.rb

  which is nice.

2020-01-09 -- literate programming with reStructuredText
--------------------------------------------------------

Reading through https://github.com/alhassy/emacs.d, which is literate
programming of emacs lisp using org-mode, I wondered what people had done for
literate programming using reStructuredText. Well, that's easy to google (or,
in fact, to duck duck go):

* https://github.com/gmilde/PyLit - last updated 8 years ago
* https://github.com/rblack42/PyLit6 - last updated 4 years ago
* https://github.com/silasdb/nw2rst - a noweb backend, last updated in 2019.
  The intent is to allow you to do::

    $ noweave -backend nw2rst.sh input.nw > output.rst

  and use the ``notangle`` program to produce the program file(s) instead.

* https://sphinx-litprog.readthedocs.io/ - a sphinx extension which introduces
  the ``.. litprog::`` directive. The repository is
  https://github.com/torfsen/sphinx-litprog,
  and it was last updated 10 months ago.

* https://repo.or.cz/pylit.git - last updated 2 years ago

* https://pythonhosted.org/rst2code/index.html and
  https://github.com/jmbarbier/rst2code, marked as archived, last updated 6
  years ago.

* https://news.ycombinator.com/item?id=10069748 -- 2015, Why did literate
  programming not catch on?

* https://leoeditor.com/history.html - the Leo editor

Hmm. So all in all, probably still a dead end.

But see also `2020-02-11`_ below, and the article on literate programming with
org-mode.

2020-01-13
----------

* http://www.dr-qubit.org/undo-tree/undo-tree.txt - the explanation of how
  normal Emacs undo works, and how undo-tree works. With ASCII art. I need
  to consider if I want to learn how to use undo-tree.

* https://nickdrozd.github.io/2019/12/28/emacs-mac-mods.html - on modifying
  Mac keyboards to give two "control" keys - less relevant for evil-mode, I
  suppose. Referenced from https://irreal.org/blog/?p=8579, which also has
  some comments.

2020-01-20
----------

* https://github.com/nmartin84/.doom.d - a Doom Emacs configuration.
* https://github.com/jacktasia/dumb-jump - an Emacs "jump to definition"
  package for 40+ languages (using rg or equivalent and a "shared set of
  heuristic methods"). Definitely something to consider.

2020-01-21
----------

* https://www.eigenbahn.com/2020/01/12/emacs-is-no-editor - with an
  explanation of what it *is* - "Emacs is a generic user-centric text
  manipulation environment."

2020-01-22
----------

From the same author:

* https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/guide-en.org
  - Mastering Emacs in one year
* https://github.com/redguardtoo/emacs.d - their ("fast and robust") Emacs setup

https://karl-voit.at/2020/01/20/start-using-orgmode/ - How to start with org-mode

2020-01-24
----------

Emacs - Procuctivity Tricks/Hacks at http://www.mycpu.org/emacs-productivity-setup/
looks rather nice.

On trying to make org-mode look nicer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* https://www.reddit.com/r/emacs/comments/estlwh/possibility_of_making_the_org_mode_less_ugly/

  Hey guys, I was introduced to emacs a year ago by my professor. I was
  wondering if there is a way to make emacs org mode less ugly for later
  views. I know it's powerful and everything and I use it on a daily basis,
  but every time I review the notes I made, I kind of want to stab myself in
  the eyes with all the #+BEGIN_SRC C and all the #blabla there is. Are there
  any ways to make it a little more beautiful?

  And yes, I know I can compile that to a latex, pdf or html, I just can't get
  the syntax highlighting

* https://www.reddit.com/r/emacs/comments/estlwh/possibility_of_making_the_org_mode_less_ugly/ffddxhn/

    **hkjels**

    Indeed. Look up prettify-symbols :) I use it to hide leading stars and
    block-wrappings such as the one you mentioned

    **celeritasCelery**

    What do you replace the block-wrapping with in prettify?

    **hkjels**

    ::

      (defun org-pretty-symbols-mode ()
          (push '("#+title: "        . "") prettify-symbols-alist)
          (push '("#+subtitle: "     . "") prettify-symbols-alist)
          (push '("* "               . "") prettify-symbols-alist)
          (push '("** "              . "") prettify-symbols-alist)
          (push '("*** "             . "") prettify-symbols-alist)
          (push '("**** "            . "") prettify-symbols-alist)
          (push '("***** "           . "") prettify-symbols-alist)
          (push '("#+author: "       . "- ") prettify-symbols-alist)
          (push '(":properties:"     . ":") prettify-symbols-alist)
          (push '("#+begin_src"      . "λ") prettify-symbols-alist)
          (push '("#+end_src"        . "⋱") prettify-symbols-alist)
          (push '("#+results:"       . "»") prettify-symbols-alist)
          (push '(":end:"            . "⋱") prettify-symbols-alist)
          (push '(":results:"        . "⋰") prettify-symbols-alist)
          (push '("#+name:"          . "-") prettify-symbols-alist)
          (push '("#+begin_example"  . "~") prettify-symbols-alist)
          (push '("#+begin_example"  . "~") prettify-symbols-alist)
          (push '("#+end_example"    . "~") prettify-symbols-alist)
          (push '("#+end_example"    . "~") prettify-symbols-alist)
          (push '("#+begin_verbatim" . "") prettify-symbols-alist)
          (push '("#+end_verbatim"   . "") prettify-symbols-alist)
          (push '("#+begin_verse"    . "") prettify-symbols-alist)
          (push '("#+end_verse"      . "") prettify-symbols-alist)
          (push '("#+begin_quote"    . "𐄚") prettify-symbols-alist)
          (push '("#+end_quote"      . "𐄚") prettify-symbols-alist)
          (push '("#+tblfm:"         . "∫") prettify-symbols-alist)
          (push '("[X]"              . (?\[ (Br . Bl) ?✓ (Br . Bl) ?\])) prettify-symbols-alist)
          (push '("\\\\"             . "↩") prettify-symbols-alist)
          (prettify-symbols-mode t))

    I also use a custom theme with mixed-pitch, so it looks quite like a
    modern word processor :)

* Also:

    **ftrx**

    About src/example block did you try something like::

      (add-hook 'org-mode-hook (lambda ()
        "Beautify Org Checkbox Symbol"
        (push '("[ ]" .  "☐") prettify-symbols-alist)
        (push '("[X]" . "☑" ) prettify-symbols-alist)
        (push '("[-]" . "❍" ) prettify-symbols-alist)
        (push '("#+BEGIN_SRC" . "↦" ) prettify-symbols-alist)
        (push '("#+END_SRC" . "⇤" ) prettify-symbols-alist)
        (push '("#+BEGIN_EXAMPLE" . "↦" ) prettify-symbols-alist)
        (push '("#+END_EXAMPLE" . "⇤" ) prettify-symbols-alist)
        (push '("#+BEGIN_QUOTE" . "↦" ) prettify-symbols-alist)
        (push '("#+END_QUOTE" . "⇤" ) prettify-symbols-alist)
        (push '("#+begin_quote" . "↦" ) prettify-symbols-alist)
        (push '("#+end_quote" . "⇤" ) prettify-symbols-alist)
        (push '("#+begin_example" . "↦" ) prettify-symbols-alist)
        (push '("#+end_example" . "⇤" ) prettify-symbols-alist)
        (push '("#+begin_src" . "↦" ) prettify-symbols-alist)
        (push '("#+end_src" . "⇤" ) prettify-symbols-alist)
        (prettify-symbols-mode)))

    Not that much but it's simple and generic, essentially you can substitute
    ANY text in an org buffer with a single character of your choice. The
    above of course being unicode demand a GUI Emacs or a terminal with
    unicode support and relative fonts...

* And "making org-mode look like a word processor":

  * https://lepisma.xyz/2017/10/28/ricing-org-mode/
  * used in https://out-of-cheese-error.netlify.com/spacemacs-config

* Also:

    **DiogenicOrder**

    My example :

    here__ and what it looks like when I don't expand headers here__.

    using:

    * `poet-theme`__
    * `variable pitch`__
    * `pretty entities`__
    * `fontify natively`__
    * `org-bullets`__
    * `highlight latex and related`__
    * more aesthetic here in `beautify org mode`__
    * ``(setq org-ellipsis "⤵")`` for style

    I'm using the Source Sans Pro for writing and Source Code Pro for code.

.. __: https://imgur.com/a/Qn6KuMZ
.. __: https://imgur.com/a/Zfk7hmn
.. __: https://github.com/kunalb/poet
.. __: https://melpa.org/#/org-variable-pitch
.. __: https://orgmode.org/manual/Special-symbols.html
.. __: https://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
.. __: https://github.com/sabof/org-bullets
.. __: http://pragmaticemacs.com/emacs/highlight-latex-text-in-org-mode/
.. __: https://mstempl.netlify.com/post/beautify-org-mode/

2020-01-28
----------

* https://donkirkby.github.io/live-py-plugin/starting_emacs - Getting started
  with Live Coding in emacs. This looks very nice. And it links to
  https://donkirkby.github.io/live-py-plugin/ which documents plugins for
  doing live Python coding in PyCharm and Sublime Text as well. Or in the
  browser.

* https://github.com/larstvei/Focus - dim the surrounding text (as you
  work). Looks nice.

* https://www.reddit.com/r/emacs/comments/etikbz/speaking_as_a_package_maintainer_please_do_not/
  - don't use MELPA stable, use normal MELPA, because it isn't actually any
  more stable. And an ensuing conversation.

* http://www.mycpu.org/emacs-productivity-setup/ - Emacs Productivity
  Tricks/Hacks: basically, use Helm, and also Evil mode, Helm-Projectile,
  maybe Doom Themes, maybe Rtags, even mu4e for email.

2020-02-03
----------

* https://github.com/whacked/calibre-mode - query calibre from emacs
* https://gitlab.com/marcowahl/keystrokes - display keystrokes within emacs

2020-02-04
==========

https://blog.abrochard.com/#/hyperbole-intro - a quick introduction to Emacs
Hyperbole.

The main page for the project is at https://www.gnu.org/software/hyperbole/

https://www.reddit.com/r/emacs/comments/euzxmx/gnu_hyperbole_708_test_release_with_patches/ffu3lum/
summarises:

  Since I see a whole bunch of comments here not understanding what this is
  (and I was just as confused), I installed it and ran the demo. Here is my
  high-level summary.

  Hyperbole is collection of about a dozen distinct packages, which is why it
  is so hard to summarize. Here are the biggest ones I noticed.

  * Hyberbole has a leader key C-h h that gives you access to all of its
    features quickly. Feels very Spacemacs-esque.
  * HyControl is a window management hydra similar to ace-window.
  * Hyrolo is similar to org-ql in that it gives you a query syntax for
    "org-like" data files.
  * Buttons. Provides a markup to define "buttons" that can be activated with
    an "action key" (M-RET). These button do things like goto definition, goto
    url, follow org link, compose email, or just run any lisp function. These
    "button" can be embedded in any buffer since they are plain text.
  * Things. A combination of objed, expand-region, and matchit. Provides
    structural editing to any buffer.

  Other notes are that it seems very well documented and has a lot of menubar
  and mouse oriented features.

(A new version is about to come out).

2020-02-05
==========

* https://thomashartmann.dev/blog/tips-and-tricks-for-the-fledgling-emacs-user/
  - Tips and tricks for the fledgling Emacs user (it is evil related as well)
* https://www.reddit.com/r/emacs/comments/eycpja/you_convinced_me_to_learn_evil_now_help/
  does seem to have some useful tips in it

2020-02-06
==========

* Projectile documentation is at https://docs.projectile.mx/en/latest/, and
  the repository is at https://github.com/bbatsov/projectile. See also
  `projects and perspectives`_ earlier in this document.

2020-03-17
==========

* https://www.reddit.com/r/emacs/comments/fh1bpg/a_variant_of_elisp_matching_paren_display/
  - displaying the matching (start) parenthesis for the (lisp) expression that
  one is inside
* https://www.reddit.com/r/emacs/comments/fiigp1/an_introduction_to_programming_in_emacs_lisp/
  - the introduction to programming in emacs lisp is now available as an epub:
  https://gitlab.com/emacs-manuals/el-intro/-/raw/master/elisp-intro.epub at
  https://gitlab.com/emacs-manuals/el-intro
* https://www.manueluberti.eu//emacs/2020/03/16/modus-themes/ "Light is right,
  alright?" a brief introduction to modus-themes - https://gitlab.com/protesilaos/modus-themes

Fish prompt - starship
----------------------

OK. Not Emacs.

https://starship.rs

::

  $ brew install starship

Starship needs powerline fonts. Let's try doing that via homebrew, as
documented at https://github.com/Homebrew/homebrew-cask-fonts ::

  $ brew tap homebrew/cask-fonts              # you only have to do this once!
  $ brew cask install font-powerline-symbols

(how does that relate to https://github.com/powerline/fonts, if at all?)

The starship configuration lives in ``~/.config/starship.toml`` - for
instance::

  # Don't print a new line at the start of the prompt
  add_newline = false

  # Or *in* the prompt
  [line_break]
  disabled = true

  # Replace the "❯" symbol in the prompt with "$ "
  [character]      # The name of the module we are configuring is "character"
  symbol = "$ "    # The "symbol" segment is being set to "➜"

  # Disable the package module, hiding it from the prompt completely
  [package]
  disabled = true

but (a) it needs initialising in my fish config to make it available
everywhere, and (b) the colours maybe don't go well with my terminal
background, and (c) it's not clear to me that it's picking up the right
symbols ... investigate more at home.

NB: ``starship prompt`` will show what prompt it would print out if it were
the current promp.

   Ah - need to tell iTerm2 to *use* that font, of course!

Maybe also of interest:

* https://dev.to/ugw7g85q/setting-up-a-dev-environment-3njm
* https://iscinumpy.gitlab.io/post/setup-a-new-mac/
* https://www.nerdfonts.com/

2020-02-10
==========

* Setting Emacs as the default text editor for all files on macOS, using duti_:
  https://posts.michaelks.org/emacs-default-macos-text-editor/ (from
  https://www.reddit.com/r/emacs/comments/f0e4yu/setting_emacs_as_the_default_text_editor_for_all/)

* Emacs 27 should be out (stable) soon. Various things in
  https://git.savannah.gnu.org/cgit/emacs.git/plain/etc/NEWS?id=emacs-27,
  but I especially note:

    **Emacs can now use the XDG convention for init files.**

    The 'XDG_CONFIG_HOME' environment variable (which defaults to
    "~/.config") specifies the XDG configuration parent directory.  Emacs
    checks for "init.el" and other configuration files inside the "emacs"
    subdirectory of 'XDG_CONFIG_HOME', i.e. "$XDG_CONFIG_HOME/emacs/init.el"

    However, Emacs will still initially look for init files in their
    traditional locations if "~/.emacs.d" or "~/.emacs" exist, even if
    "$XDG_CONFIG_HOME/emacs" also exists.  This means that you must delete
    or rename any existing "~/.emacs.d" and "~/.emacs" to enable use of
    the XDG directory.

    If "~/.emacs.d" does not exist, and Emacs has decided to use it
    (i.e. "$XDG_CONFIG_HOME/emacs" does not exist), Emacs will create it.
    Emacs will never create "$XDG_CONFIG_HOME/emacs".

    Whichever directory Emacs decides to use, it will set
    'user-emacs-directory' to point to it.

  so I'll need to decide what (if anything) I want to do about that. I have
  not read through the entire NEWS file - it's very long.

* https://with-emacs.com/posts/tutorials/almost-all-you-need-to-know-about-variables/
  - (almost) all you need to know about [elisp] variables.

* https://github.com/wasamasa/eyebrowse - another window configuration
  manager. See https://manuel-uberti.github.io/emacs/2017/08/06/eyebrowse/ for
  an example of how someone uses it.

.. _duti: https://github.com/moretension/duti

2020-02-11
==========

* http://www.howardism.org/Technical/Emacs/literate-programming-tutorial.html -
  an Introduction to Literate Programming, using org-mode. Intended as a
  workshop that one can follow along with. And thus also an introduction to
  org-mode itself.

* https://www.reddit.com/r/emacs/comments/f1jmwn/newbie_question_how_do_you_manage_your_packages/
  - how do you manage your packages across computers

  1. Use ``straight.el``
  2. Use ``use-package``:

        If you set ensure to t then it will install the package if its not
        already installed::

          (use-package foo
          :ensure t)

     and remember to make ``use-package`` get installed automatically::

          (package-initialize)
          (if (not (package-installed-p 'use-package))
              (progn
                (package-refresh-contents)
                (package-install 'use-package)))
          (require 'use-package)

     or (to avoid ``(if (not``)::

          (package-initialize)
          (unless (package-installed-p 'use-package)
            (package-refresh-contents)
            (package-install 'use-package))
          (require 'use-package)

  3. Put everything under version control (!)

* https://thomashartmann.dev/blog/tips-and-tricks-for-the-fledgling-emacs-user/
  - Tips and tricks for the fledgling Emacs user: Notes from my journey
  through the manual

  Recommends https://www.masteringemacs.org/ - Mastering Emacs (eBook) $39.99

2020-02-14
==========

That thing where I'm still using vim for edits in the terminal, in particular
as the setting for ``$EDITOR``. I've said before that I should fix that to
allow emacs to work properly for me. But this article
https://blog.aaronbieber.com/2016/12/29/don-t-use-terminal-emacs.html
would have it that I should just use GUI emacs:

  If you’ve made it this far without cheating, you already know my opinion. I
  strongly advise you to simply stop using Emacs in the terminal. Full stop.

  You may ask, “but what if I need to edit my crontab file or something?”
  Sure. I get that. Guess what, you can use GUI Emacs for that, too, if you
  have ``(server-start)`` in your init file and your ``$VISUAL`` environment
  variable is set to emacsclient.

  Live in the blissful world of 16.7 million colors, different font sizes, and
  infinite key bindings. Live in the GUI, forever.

Now, it's not quite that simple, because I deliberately *haven't* put
``(server-start)`` in my init file, just so I *can* start up a separate emacs
instance (e.g., for isolated testing). But I could presumably set ``$VISUAL``
to be the command I *do* use for starting up an emacs client if necessary and
then doing emacs.

---------

* https://github.com/pezra/rspec-mode/ - RSpec mode
* https://thomashartmann.dev/blog/my-first-emacs-lisp/ - My first Emacs Lisp
  (Or: How I can't let Vim go) - writing his first emacs lisp functions so he
  could do something he misses from vim.

* https://www.reddit.com/r/emacs/comments/f1ayo1/magit_is_like_git_at_the_speed_of_thought/ -
  example of using magit to do a particular (set of) tasks

* Note emacs: ``brew install diffoscope``

  https://diffoscope.org/ from the `Reproducible Builds`_ project.

    """In-depth comparison of files, archives, and directories.

    diffoscope will try to get to the bottom of what makes files or
    directories different. It will recursively unpack archives of many kinds
    and transform various binary formats into more human readable form to
    compare them. It can compare two tarballs, ISO images, or PDF just as
    easily."""

.. _`Reproducible Builds`: https://reproducible-builds.org

2020-02-24
==========

* https://github.com/terlar/indent-info.el - Show indentation information in
  the status bar (spaces/tabs and tab width)
* https://github.com/emacs-dashboard/emacs-dashboard/ - a startup screen for
  emacs that shows potentially useful information. Not sure this is relevant
  to how I work...
* https://oremacs.com/2015/01/17/setting-up-ediff/ - setting up ediff

2020-02-26
==========

* https://opensource.com/article/20/2/who-cares-about-emacs - Who cares about
  Emacs? - a nice little article.

2020-02-27
==========

* https://www.reddit.com/r/emacs/comments/f9cak3/emacsformacosxcom_nightlies/

    **lawlist** 1 day ago

    ``git clone -b master git://git.sv.gnu.org/emacs.git`` ... ``cd`` to the
    ``emacs`` directory of the repository, ``./autogen.sh, ./configure``
    [insert options/flags here], ``make, make install`` ... if needed, set the
    ``$PATH`` beforehand as well as ``CPPFLAGS``, ``LDFLAGS``, ``CFLAGS``,
    ... to update the the local repository, ``git pull`` .... The building
    process requires certain prerequisites such as recent versions of
    ``autoconf`` and ``automake`` and some other goodies depending upon which
    version of OSX / MacOS is being used.

    **dixius99** 1 day ago

    Same, but your clone will pull Emacs 28, right? For 27, I think it would be:

    ``git clone -b emacs-27 git://git.sv.gnu.org/emacs.git``

    **onetom** 1 day ago

    ``brew install d12frosted/emacs-plus/emacs-plus --HEAD`` for building v28
    and optionally a ``--with-emacs-27-branch`` for building v27

2020-03-02
==========

https://www.reddit.com/r/emacs/comments/faz1fm/seeing_a_flashing_screen_every_once_in_a_while_on/
seems related to the "window flashing white" problem I observe with emacs on
my macs, and which is so hard to search online for(!).

2020-03-10
==========

* https://blog.binchen.org/posts/how-to-speed-up-lsp-mode.html - How to speed
  up LSP mode. But it's also a complete LSP setup, with documentation, so
  might be useful if I decide to try LSP instead of eglot. It does, however,
  remove in-emacs linting, which I think might be a problem.

2020-03-26
==========

* https://www.reddit.com/r/emacs/comments/fme6h2/the_gnu_emacs_lisp_reference_manual_in_epub_format/

  * https://gitlab.com/emacs-manuals/refer-el
  * https://gitlab.com/emacs-manuals/refer-el/-/raw/master/GELRM.epub

* 2020 https://thomashartmann.dev/blog/org-mode-tasty-tricks/ - org-mode tasty tricks

* 2020 http://xenodium.com/modern-elisp-libraries/index.html - modern
  emacs-lisp libraries - i.e., libraries of functions to help with programming
  elisp datatructures, etc., etc.

2020-03-27
==========

https://yoo2080.wordpress.com/2014/07/04/it-is-not-hard-to-read-lisp-code/ -
It is not hard to read Lisp code

2020-03-30
==========

* http://xenodium.com/string-inflection-emacs-package/index.html - cycle through
  camelCase, snake_case, etc. Repository at https://github.com/akicho8/string-inflection

* https://github.com/mihaiolteanu/vuiet - Vuiet, the music player and explorer
  for Emacs. Depends on https://github.com/mihaiolteanu/lastfm.el, and also
  https://github.com/ytdl-org/youtube-dl/ and https://mpv.io/

2020-04-01
==========

https://www.aidanscannell.com/post/setting-up-an-emacs-playground-on-mac/
Setting Up an Emacs Playground on MacOS - Emacs Mac Port | Chemacs |
Emacsclient | Spacemacs

2019 https://ylluminarious.github.io/2019/05/23/emacs-mac-port-introduction/ -
Emacs Mac Port Introduction

The Emacs Mac Port itself is at https://bitbucket.org/mituharu/emacs-mac/
and its README is at https://bitbucket.org/mituharu/emacs-mac/src/master/README-mac

2020-04-06
==========

https://www.manueluberti.eu/emacs/2020/04/06/lockdown-beam-hide-mode-line/
which leads me to

* 2018 https://www.manueluberti.eu/emacs/2018/03/10/moody-and-minions/ Beauty
  lies in the segments of the mode line - definitely worth following up
* 2020 https://www.manueluberti.eu/emacs/2020/03/01/helm-ripgrep-mode-line/
  Ripgrepping with Helm: the mode-line (less directly interesting)

and

* https://github.com/tarsius/minions - a minor-mode menu for the mode line
* https://github.com/tarsius/moody - tabs and ribbons for the mode-line
* https://www.manueluberti.eu/emacs/2017/08/06/eyebrowse/ - show the
  https://github.com/bbatsov/projectile workspace number in the modeline

2020-04-09
==========

* https://morioh.com/p/1edf2082062a - Emacs: The Best Python Editor? - how to
  set up Emacs for working with Python.

2020-04-22
==========

* https://github.com/wandersoncferreira/dotfiles - Literate Emacs
  configuration with EXWM http://www.bartuka.com

* https://github.com/gexplorer/simple-modeline - A simple mode-line for Emacs.
  According to
  https://www.reddit.com/r/emacs/comments/g3k54a/a_simple_modeline_configuration_for_emacs/
  the font is https://typeof.net/Iosevka/

* https://medium.com/better-programming/15-reasons-why-i-use-emacs-with-gifs-5b03c6608b61 -
  15 reasons why I use Emacs, with GIFs

* And it's vim, not evil, but https://www.hillelwayne.com/post/intermediate-vim/
  (31 May 2019) is quite a nice summary of things-that-make-vim-useful

2020-04-30
==========

https://andreyorst.gitlab.io/posts/2020-04-29-text-editors/ - the author's
take on comparing text editors, and why emacs is (mostly) the best. But it's a
broader take than normal on this, and has some interesting arguments.

2020-04-28
==========

* https://eamonnsullivan.co.uk/posts-output/2020-04-25-remote-first-emacs/
  - Remote First, Pair Programming and Emacs. Some hints on how to pair
  program using Emacs, especially when the "pair" is not an emacs user.

  I think the hydra they present sounds very useful. And might be a reason to
  get to know hydra.

2020-04-28 Pomodoro stuff
=========================

* https://francescocirillo.com/pages/pomodoro-technique - an explanation of
  the pomodoro techique (which I'm meant to be looking into, anyway)
* https://github.com/abo-abo/hydra - hydra itself
* https://github.com/fniessen/emacs-leuven-theme - the Emacs theme they
  like, especially when using a small screen over a remove link. It does
  look nice at a first glance
* https://github.com/TatriX/pomidor - a pomodoro time for emacs
* https://github.com/eamonnsullivan/emacs.d - their emacs configuration,
  which they included in case of interest, but aren't particularly pushing
  (I'll link it separately elsewhere as well).

2020-05-04
==========

* http://karolis.koncevicius.lt/posts/porn_zen_and_vimrc/ - "Porn, Zen and
  .vimrc". The story of how they used their .vimrc to "mend" some of the
  "slightly broken" commands in vim, and later learnt why this might not be a
  good idea after all, and reverted to a very short .vimrc. It's a nice
  article.

2020-05-05
==========

* https://www.eliasstorms.net/zetteldeft/ - Introducing Zetteldeft, a
  Zettelkasten for Emacs

  Zetteldeft is an extension of the Deft package for Emacs. Building on Deft’s
  search functionality, Zetteldeft provides a way to create and manage links
  between short notes.

2020-05-11
==========

* https://gitlab.com/matsievskiysv/multistate - "multistate-mode is basically
  an evil-mode without vi. It allows to define states for modal editing.
  Unlike evil-mode it doesn't come with predefined states and key bindings,
  doesn't mess with system hooks so that you could configure your system the
  way you want."

  Interesting. The examples don't seem to include the equvalent of ``:``.

* https://github.com/chenyanming/calibredb.el - Yet another calibre emacs
  client. Inspired by calibre-mode, this package integrates calibre (using
  calibredb) into emacs.

2020-05-13
==========

* https://menno.io/posts/use-package/ - nice intro to use-package, and its
  examples are very clear.

2020-06-05
==========

A while since I've done some of these.

* https://www.reddit.com/r/emacs/comments/gw4ok1/what_are_your_top_navigation_aids_and_why/
  - a really useful list and discussion of why one might want to use
  particular add-ons (mostly the usual suspects, but that's OK)

* https://spwhitton.name/blog/entry/transient-mark-mode/ - I don't
  particularly understand the issues to hand, but that's why I've kept this
  link!

* https://www.miskatonic.org/2020/05/28/emacs-refactoring/ - mainly talking
  about taking on use-package, with a couple of useful tips.

* https://degruchy.org/2020/05/26/binding-emacs/ - "In which I write about how
  I bind my key maps in Emacs". Again, mostly about how this can be done in
  the context of use-package.

* https://github.com/chenyanming/calibredb.el - Yet Another calibre client.

* https://github.com/plexus/chemacs - an Emacs profile switcher - "it makes it
  easy to run multiple Emacs configurations side by side. Think of it as a
  bootloader for Emacs."

  The Doom documentation also talks about using it, in the context of allowing
  the user to experiment with Doom: https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#install-doom-alongside-other-configs-with-chemacs

and OK, not emacs or vim, but:

* https://latex.now.sh/ - This almost class-less CSS library turns your HTML
  document into a website that looks like a LaTeX document.

2020-06-10
==========

* https://emacsredux.com/blog/2020/06/10/comment-commands-redux/ - commands
  for comenting/uncommenting code. I really must figure out if evil has
  special bindings for these (and are they the same as the vim bindings for
  the similar funcitonality in vim, which I never learnt either!)

2020-06-12
==========

* Lisp, not Emacs Lisp, and old lisp at that:
  https://t3x.org/lisp64k/index.html - LISPy things you can do in 64K bytes of
  core ("the amount of space your LISP system would get on a basic DEC PDP-11
  ... probably a little less space than the first implementations of LISP
  could use").

* https://github.com/phikal/dumb-jump again - "an Emacs "jump to definition"
  package with support for 40+ programming languages that favors "just
  working". This means minimal -- and ideally zero -- configuration with
  absolutely no stored indexes (TAGS) or persistent background processes."

2020-06-19
==========

* https://github.com/eamonnsullivan/emacs.d - referenced elsewhere as well

2020-06-24
==========

I'm running emacs 26.3, via a homebrew cask::

  $ brew cask list emacs
  ==> Apps
  /Applications/Emacs.app (4,039 files, 166.9MB)
  ==> Binary Symlinks
  /usr/local/bin/emacs -> /Applications/Emacs.app/Contents/MacOS/Emacs (3.3KB)
  /usr/local/bin/ebrowse -> /Applications/Emacs.app/Contents/MacOS/bin/ebrowse (73.4KB)
  /usr/local/bin/emacsclient -> /Applications/Emacs.app/Contents/MacOS/bin/emacsclient (51.3KB)
  /usr/local/bin/etags -> /Applications/Emacs.app/Contents/MacOS/bin/etags (160.9KB)

which is using https://emacsformacosx.com/, presumably via
https://github.com/Homebrew/homebrew-cask/blob/master/Casks/emacs.rb, which
just does::

  cask 'emacs' do
    version '26.3'
    sha256 '310f34b3890584b08fd6de9f422d0747ed3405910120ecfd2eb2cbf8921986a6'

    url "https://emacsformacosx.com/emacs-builds/Emacs-#{version}-universal.dmg"
    appcast 'https://emacsformacosx.com/atom/release'
    name 'Emacs'
    homepage 'https://emacsformacosx.com/'

    conflicts_with formula: 'emacs'

    app 'Emacs.app'
    binary "#{appdir}/Emacs.app/Contents/MacOS/Emacs", target: 'emacs'
    binary "#{appdir}/Emacs.app/Contents/MacOS/bin/ebrowse"
    binary "#{appdir}/Emacs.app/Contents/MacOS/bin/emacsclient"
    binary "#{appdir}/Emacs.app/Contents/MacOS/bin/etags"

    zap trash: [
                '~/Library/Caches/org.gnu.Emacs',
                '~/Library/Preferences/org.gnu.Emacs.plist',
                '~/Library/Saved Application State/org.gnu.Emacs.savedState',
              ]
  end

The article
https://fossegr.im/emacs/exploring-emacs/2020/06/21/exploring-emacs-II-installation-and-basic-usage.html
describes doing::

  brew tap d12frosted/emacs-plus &&
  brew install emacs-plus@27 --with-modern-icon-cg433n &&
  ln -s /usr/local/opt/emacs-plus@27/Emacs.app /Applications

to install emacs 27, from a different cask,
https://github.com/d12frosted/homebrew-emacs-plus
and says that this "offers a wide rage of extra functionality over regular →
Emacs package." where that "regular Emacs package" is
https://formulae.brew.sh/formula/emacs#default
which is implemented by
https://github.com/Homebrew/homebrew-core/blob/master/Formula/emacs.rb
and appears to build Emacs from source.

Apart from the ability to install 2.7 (or 2.8), it's not entirely clear what
difference/advantage (if any) there is with respect to what I'm currently using.

2020-06-25
==========

Still not sure about Deft (https://jblevins.org/projects/deft/)

However, it should be possible to make it work with reStructuredText files,
because (from that page):

  The displayed title of each file is taken to be the first line of the file,
  with certain characters removed from the beginning.  Hash characters, as
  used in Markdown headers, and asterisks, as in Org Mode headers, are
  removed.  Additionally, Org mode ``#+TITLE:`` tags, MultiMarkdown ``Title:``
  tags, LaTeX comment markers, and Emacs mode-line declarations (e.g.,
  ``-*-mode-*-``) are stripped from displayed titles.  This can be customized
  by changing ``deft-strip-title-regexp``.

  More generally, the title post-processing function itself can be customized
  by setting ``deft-parse-title-function``, which accepts the first line of the
  file as an argument and returns the parsed title to display in the file
  browser.  The default function is ``deft-strip-title``, which removes all
  occurrences of ``deft-strip-title-regexp`` as described above.

(and the ``deft-strip-title-regexp`` function even has an example of stripping
any line that is just composed of ``#`` characters).

2020-07-06
==========

* https://tech.toryanderson.com/2020/07/03/emacs-tip-registers/ - some nice,
  simple, examples of using registers.

2020-07-16
==========

Hah! I had cause to use::

  .. Local<space>Variables:
  .. mode: text
  .. fill-column: 200
  .. End:

for the first time in ages! (note that the ``<space>`` should, of course, be a
space, but if I don't do that, emacs is likely to read it as instructions for
this file!).

2020-07-20
==========

Lisp and types and such

* https://alhassy.github.io/TypedLisp.html - Typed Lisp, a Primer, which also
  specifically talks about emacs Lisp towards the end (2019)
* https://lispcookbook.github.io/cl-cookbook/type.html - the section on "Type
  System" from The Common Lisp Cookbook

2020-07-21
==========

Still on Lisp:

* http://jakob.space/blog/thoughts-on-lisps.html - The Many Faces of an
  Undying Programming Language (2020)

2020-08-07
==========

* https://kakoune.org/ -- a different approach to modal editing than vim
  (noun then verb, not verb then noun)

* https://irreal.org/blog/?p=9058 - Roll your own Emacs modal mode which talks
  about

  * https://llazarek.com/2018/07/modal-editing-in-emacs.html - Modal editing
    in Emacs - why it's easy to implement, and can be much better than in vim.

* https://archive.casouri.cat/note/2020/simple-(back)-links-in-any-file/index.html -
  automatic back links in text files without having to use org-mode.

* https://github.com/hamnixster/ft-leader - provide leader access to any
  existing emacs keybindigs.

* https://github.com/Alexander-Miller/treemacs - a tree layout file explorer
  for emacs

* https://puntoblogspot.blogspot.com/2018/05/til-elisp-has-iterators.html -
  iterators in emacs lisp (and generators)

* https://github.com/Wilfred/helpful - a better (nicer?) help that provides
  more contextual information than the default emacs help

* https://github.com/justbur/emacs-which-key - a minor mode for Emacs that
  displays the key bindings following your currently entered incomplete
  command (a prefix) in a popup

* https://lccambiaghi.github.io/.doom.d/readme.html - documentation for Luca
  Camiaghi's DOOM emacs configuration.

* https://www.manueluberti.eu//emacs/2020/07/24/paper/ - making emacs look
  like paper? Background they like is too white for me.

* https://with-emacs.com/posts/tutorials/what-you-need-to-know-about-hooks/ -
  2020, What you Need to Know About Hooks

* https://emacs.nasy.moe/ - another emacs configuration

And, of ESPECIAL INTEREST:

* el-get: https://www.emacswiki.org/emacs/el-get and https://github.com/dimitri/el-get
  - a tool for handling emacs lisp files that aren't necessarily in melpa, etc.

* https://github.com/glasserc/ethan-wspace - "the definitive emacs
  customizations for people who are OCD about whitespace." This may be exactly
  what I want...

* https://github.com/phikal/dumb-jump - an Emacs "jump to definition" package
  with support for 40+ programming languages that favors "just working".

2020-08-13
==========

* https://lists.gnu.org/archive/html/emacs-devel/2020-08/msg00237.html - Emacs
  27.1 released. Refer back to `2020-02-10`_ for anticipation, and also to
  `2020-02-27`_  for how to clone and build it, and `2020-06-24`_ for how to
  use homebrew to install different versions of Emacs, not just 27 (I
  definitely want to investigate this as an alternative to the cask I
  currently use).

  But it also looks as if the cask I normally use may already have upgraded::

    $ brew cask info emacs
    emacs: 27.1-1
    https://emacsformacosx.com/
    /usr/local/Caskroom/emacs/26.3 (5 files, 269.0KB)
    From: https://github.com/Homebrew/homebrew-cask/blob/HEAD/Casks/emacs.rb
    ==> Name
    Emacs
    ==> Artifacts
    Emacs.app (App)
    /Applications/Emacs.app/Contents/MacOS/Emacs -> emacs (Binary)
    /Applications/Emacs.app/Contents/MacOS/bin/ebrowse (Binary)
    /Applications/Emacs.app/Contents/MacOS/bin/emacsclient (Binary)
    /Applications/Emacs.app/Contents/MacOS/bin/etags (Binary)
    ==> Analytics
    install: 3,795 (30 days), 11,950 (90 days), 52,316 (365 days)

* https://www.reddit.com/r/emacs/comments/i6ebl8/lspmode_looks_cool_but_how_do_you_set_it_up/
  - advise on setting up LSP mode.

* https://llazarek.com/2018/07/modal-editing-in-emacs.html - 2018, Model
  Editing in Emacs (is easy, and can be much better than vim). It would be
  interesting to know what has changed since this was written.

2020-08-18
==========

Homebrew updated me to emacs 27.1 (actually, yesterday, but I only restarted
my emacs server today).

2020-08-21
==========

* https://www.masteringemacs.org/article/whats-new-in-emacs-27-1 - essentially
  the NEWS file with more detail on some of the items.

* https://github.com/corvideon/mousemacs -  an Emacs setup for those who want
  the power of Emacs but also want to use a mouse. Probably not of direct
  interest, but some of the things it does may be useful, in particular the
  various context menus.

* https://oremacs.com/2015/01/17/setting-up-ediff/ - from 2015

2020-08-27
==========

* https://github.com/tychoish/.emacs.d - Tycho Emacs Config Kit. The
  "Background" introduction says:

    I've been using Emacs for a long time, and I suspect I will be for a while
    yet. This repository contains all of that configuration, packaged up in a
    way that anyone can use, learn from, and get started using very
    quickly. There are a lot of starter kits for emacs, and this isn't quite
    that; although if you're newer to Emacs and want to bootstrap the
    configuration process, or if you've been using Emacs a bit for a while but
    aren't particularly fond of your configuration, this might be a good place
    to start.

    Fair warning, where starter kits are flashy and opinionated, this
    configuration strives for a exceptionally minimal and lightweight
    experience that's also fully usable and feature complete as your primary
    text-editing and software development toolkit.

* https://medium.com/urbint-engineering/emacs-doom-for-newbies-1f8038604e3b
  - Emacs Doom for newbies, 2017. Mostly obvious(ish) stuff, but the
  description of using projectile to setup/open projects is useful.

2020-09-03
==========

* https://www.reddit.com/r/emacs/comments/ij3d7u/emacsapp_for_os_x_needs_two_clicks_to_open_menus/
  - apparently a "System Preferences > Security and Privacy > Privacy >
  Accessibility" issue.

* https://cestlaz.github.io/post/using-emacs-74-eglot/ - Using Emacs 74 Eglot

* https://www.reddit.com/r/emacs/comments/ijmgtx/tip_how_to_use_a_stable_and_fast_environment_to/
  - TIP: How to use a stable and fast environment to develop in Python

* https://with-emacs.com/posts/tutorials/customize-completion-at-point/ -
  Customize completion-at-point (tutorial)

2020-09-04
==========

* https://www.rousette.org.uk/archives/advising-emacs/ - using the ‘advice’
  system to customise commands, and other things. From the " but she's a
  girl..." blog.

2020-09-07
==========

* https://blog.mudit.xyz/posts/angels-and-daemons-a-tale-of-emojis-in-emacs -
  Angels and daemons — a tale of emojis in emacs - or why an emoji in a text
  file could crash emacs

* https://www.masteringemacs.org/article/unicode-ligatures-color-emoji -
  Unicode, Ligatures and Color Emoji (in emacs 2.7)

2020-09-18
==========

* https://github.com/ymherklotz/emacs-zettelkasten - Zettelkasten
  mode. Doesn't have to be org files.

* https://emacsredux.com/blog/2020/09/15/emacs-prelude-1-0/ - Emacs Prelude
  1.0 released. Mostly just a cosmetic renumbering, but the project is 9 years
  old and stable. From the note:

    its core philosophy:

    * simple
    * easy to understand and extend
    * stable
    * a foundation for you to build upon, as opposed to some end-user product
      that you’re supposed to use as-is

* https://github.com/purcell/whole-line-or-region - This minor mode allows
  functions to operate on the current line if they would normally operate on a
  region and region is currently undefined.

* Older links to things:

  * http://irreal.org/blog/?p=5095 - Some Oldies but Goodies (2016)
  * http://irreal.org/blog/?p=6419 - Some Oldies but Goodies Updated (2017)

  which may mention whole-line-or-region

* http://irreal.org/blog/?p=6419 - Useful Emacs Lisp Scripts

* https://www.jetbrains.com/lp/mono/ - JetBrains mono (text font)

2020-09-28
==========

* https://www.manueluberti.eu//emacs/2020/09/18/project/ - why project.el
  might be enough, and some customisation to use it.

* https://github.com/rejeep/f.el - Emacs Lisp functions for working with files
  and directories. Meant to be nicer and easier to use than what emacs lisp
  comes with.

2020-09-29
==========

* https://github.com/grimnebulin/emacs-kanji-quiz - This package provides
  commands which process a buffer of Japanese vocabulary terms in a simple
  format, and present those terms to the user in a simple flashcard-like
  format.
* https://github.com/larsbrinkhoff/emacs-history - Historical Emacs Software
  Preservation - implementations of emacs from 1976 (TECO EMACS) through to
  current Emacs.
* https://github.com/amno1/as-powerful-as-possible - "the English translation
  of the book

    Moćan koliko je god moguće by Kazimir Majorinc

  The book itself is about the origins and the first few years of development
  of Lisp, the programming language." Still in progress.

2020-10-07
==========

* http://mbork.pl/2017-01-14_I'm_now_using_the_right_dictionary - Getting set
  up to use Webster's dictionary in Emacs

  * https://jsomers.net/blog/dictionary - You’re probably using the wrong
    dictionary (2014) on the virtues of using Webster's 1913 dictionary

* https://emacspeak.blogspot.com/2020/10/on-defining-keys-in-emacs.html - on
  defining keys in emacs, and the linked
  https://github.com/tvraman/emacspeak/blob/master/lisp/ems-kbd.el#L1
  which provides (the first version of?) some code to help. Not a finished
  project.

* https://github.com/tilmanrassy/emacs-dir-treeview - Emacs file system
  navigator and simple file manager

* https://ethanaa.com/blog/switching-to-doom-emacs/ - Switching to Doom Emacs,
  2020. The comments suggest using more "evil-ish" key bindings for some things.

* https://systemhalted.in/emacs-resources/ - a list of links

* Handling long lines in Emacs (2020)
  https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html

* https://github.com/200ok-ch/organice/ - an implementation of Org mode
  without the dependency of Emacs. It is built for mobile and desktop browsers
  and syncs with Dropbox, Google Drive and WebDAV.

* https://www.reddit.com/r/emacs/comments/j3oczn/how_do_you_navigate_around_several_files_while/
  - How do *you* navigate around several files while programming?

* https://www.reddit.com/r/emacs/comments/j2xezg/usepackage_best_practices/ -
  ``use-package`` best practices?

2020-10-08
==========

* https://protesilaos.com/codelog/2020-10-08-intro-usls-emacs-notes/ - My
  simple note-taking system for Emacs (without Org)

2020-10-13
==========

https://emacsredux.com/blog/2020/09/12/reinstalling-emacs-packages/

  Sep 12, 2020 | Utilities • Bozhidar Batsov

  From time to time you might run into issues with packages that are not
  properly byte-compiled when installed via package.el (and use-package by
  association). This may manifest itself in many different ways - typically
  something being nil/undefined when it shouldn’t be. Here’s a small utility
  that might help you in such situations::

    (defun er-reinstall-package (pkg)
      (unload-feature pkg)
      (package-reinstall pkg)
      (require pkg))

  I hope you’ll agree that the function’s definition is pretty
  self-explanatory. Now you can do things like::

    ;; try running this code in ielm or a scratch buffer
    (er-reinstall-package 'crux)
    (er-reinstall-package 'projectile)

  As one reader suggested we can make the function a bit more user friendly by
  making it an interactive command that you can invoke with M-x::

    (defun er-reinstall-package (pkg)
      (interactive (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
      (unload-feature pkg)
      (package-reinstall pkg)
      (require pkg))

  Now you’ll also get prompted for the package to reinstall with some
  convenient completion.

  I assume that’s not a function you’d need often, but it might come in handy
  from time to time. That’s all I have for you today. Keep hacking!

.. note::

   I really need to look at https://github.com/hlissner/doom-emacs again - it
   keeps moving on. Apparently there's a very good Discord community, as well:
   https://discord.gg/qvGgnVx

   And, as recommended in the FAQ, https://github.com/plexus/chemacs is my
   friend.

   Of course, I should also rework how I store/manage my dotfiles and suchlike
   (so many things to do...)

2020-10-26
==========

General lisp advocacy:

* https://www.defmacro.org/ramblings/lisp.html - The nature of Lisp, 2006

  Explains Lisp by starting with XML and working backwards through (e.g., Ant)
  as representations of programs.

* https://stopa.io/post/265 - An intuition for Lisp Syntax (undated)

  Starts with JavaScript, "invents" an array based representation for user
  code.

* https://stopa.io/post/222 - Risp (in (Rust) (Lisp)) - writing a simple Lisp
  interpreted in Rust, inspired by https://norvig.com/lispy.html - (How to
  Write a (Lisp) Interpreter (in Python))

* https://dustycloud.org/blog/wisp-lisp-alternative/ - Wisp: Lisp, minus the
  parentheses (2015) - a proposal to write Lisp with significant indentation
  instead of parentheses

* http://chriswarbo.net/blog/2017-08-29-s_expressions.html -
  S-expressions (2017) - the author proposes making the parentheses lower
  contrast, so you can't see them. I'm not convinced by their example, but
  still it's an idea - see the "Wisp" post above.

* https://t3x.org/lfn/index.html - the book "Lisp for Nothing", which I have a
  physical copy of. There is other stuff by the same author at the website,
  some of which also looks interesting (more Lisp/Scheme implementations).

Emacs:

* https://tech.tonyballantyne.com/emacs/pattern-matching-pcase/ - pattern
  matching in emacs lisp with ``pcase``. 2020

* https://github.com/magnars/expand-region.el - Expand region increases the
  selected region by semantic units. Just keep pressing the key until it
  selects what you want.

Doom (again):

* https://github.com/hlissner/emacs-doom-themes/tree/screenshots at
  https://github.com/hlissner/emacs-doom-themes - an opinionated UI plugin and
  pack of themes. The light themes actually look very nice.

* https://github.com/hlissner/doom-emacs itself

* https://github.com/plexus/chemacs to manage multiple emacs installations
  (or, as the Doom FAQ says, "think of it as a bootloader for Emacs". Not sure
  how I feel about that analogy).

* https://www.gtrun.org/post/config/ - GuangTao's Doom Emacs config (2020)

* https://gist.github.com/kid1412z/3f687ace33a089f3613cce41660d210b - may or
  may not be up-to-date (suspect not) - Doom cheatsheet (keybindings)

  Of course, one can also look at
  https://github.com/hlissner/doom-emacs/blob/develop/modules/config/default/+evil-bindings.el
  and try to understand what that is setting (line 253 or so is the
  ``;;; <leader>`` section). Hmm - it looks as if there's some nice stuff in
  there, and this might actually be a way of learning more emacs/evil commands.

* https://tecosaur.github.io/emacs-config/config.html - Doom Emacs
  Configuration: The Methods, Management, and Menagerie of Madness (2020) - at
  least this page itself looks nice! There's a *lot* of stuff in here(!) -
  inevitably a lot of it for org-mode.

  Hmm. The emoji support might actually be useful...

* https://rgoswami.me/dotdoom/config.html - Literate doom-emacs config (2020)

**NOTE** that:

* Doom can keep its configuration in ~/.config/doom
* As of Emacs 27, enacs can keep its configuration in ~/.config/emacs
* Sadly, vim doesn't seem as friendly - it still wants ~/.vimrc
* More sadly, it doesn't look as if chemacs supports XDG (~/.config) locations

The advice on "syncing" doom setup between computers (from the FAQ) says to
just sync ~/.doom.d - and *not* to sync ~/.emacs.d (because there are
transient/local directories kept in there).

**NOTE** that I've currently got installed::

  $ brew cask info emacs
  emacs: 27.1-1
  https://emacsformacosx.com/
  /usr/local/Caskroom/emacs/27.1-1 (5 files, 269.1KB)
  From: https://github.com/Homebrew/homebrew-cask/blob/HEAD/Casks/emacs.rb

and the Doom Emacs guide, at
https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#with-homebrew
suggests that this is not the best solution for use with Doom. It recommends
using emacs-plus, which I'd been wondering about (elsewhere above) anyway.

* https://github.com/d12frosted/homebrew-emacs-plus

Hmm. I need a plan...

2021-01-14
==========

Interesting from slack:

* "in any buffer you can use C-z  to toggle between evil-mode and standard
  emacs keybindings" - I think I knew that, and indeed I think I "fell over"
  it just recently!

* "in any buffer you can M-: major-mode  to get the name of the current major
  mode" - but I definitely didn't know this one

2021-01-19
==========

* https://irreal.org/blog/?p=9418 - "Yesterday’s Date" - or, rather, the date
  from N days ago. Specific code::

    1: (defun jcs-yesterday (days)
    2:   "Insert yesterday's date."
    3:   (interactive "p")
    4:   (let ((ts (decode-time)))
    5:     (setf (nth 3 ts) (- (nth 3 ts) (if days days 1)))
    6:     (insert (format-time-string "%Y-%m-%d" (apply #'encode-time ts)))))

* https://github.com/peterwu/plus-minus - "This package attempts to offer
  plus-minus operations on numbers in a similar fashion to how Vim handles
  them."

* https://emacsredux.com/blog/2020/12/10/essential-magit-file-commands/ -
  "Essential magit file commands"

* https://github.com/alphapapa/unpackaged.el - "A collection of useful Emacs
  Lisp code that isn’t substantial enough to be packaged."

* https://www.youtube.com/playlist?list=PLhXZp00uXBk4np17N39WvB80zgxlZfVwj -
  DoomCasts: Emacs Doom Screencasts. "In this series I will go over most of
  the packages included in Emacs Doom by default."

* Someone answered "are there any other editors as programmable as emacs" by
  pointing to https://github.com/lem-project/lem, which is written in Common
  Lisp. Appears to have at least some vi[m] compatibility, from the
  ``lem-vi-mode`` entries in the example init.lisp files. Some of the wiki
  pages seem to be in Japanese. Unsurprisingly, the first aim seems to be to
  able to edit Common Lisp...

2021-02-08
==========

* https://lisp-journey.gitlab.io/blog/state-of-the-common-lisp-ecosystem-2020/
  - State of the Common Lisp ecosystem, 2020 (as of Jan/Feb 2021)

  Opinionated, and looks very good.

* https://github.com/karthink/project-x - "Project-X adds a couple of
  convenience features for Emacs’ project.el library."

  * Recognize any directory with a .project file as a project. Also works if
    any parent directory has this file.
  * Save and restore project files and window configurations across sessions.

* https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/ - Using the
  tab-bar in Emacs (from "but she's a girl...")

* https://countvajhula.com/2021/01/21/vim-tip-of-the-day-a-series/ - Vim Tip
  of the Day - a series. This is the top page for the series. Note that it
  explicitly also attempts to work with Evil.

2021-02-09
==========

* https://github.com/sharkdp/fd#integration-with-other-programs - using ``fd``
  with other programs, such as emacs

2021-02-15
==========

* https://irreal.org/blog/?p=9482 - Fixing two spaces between sentences.
  Actually interesting because it mentions::

    (setq sentence-end-double-space nil) ;period single space ends sentence

  which is the way to tell Emacs that there should be *one* space after the
  end of a sentence.

  (I've added that to my config.el)

* https://www.dr-qubit.org/Descent_into_Evil.html - Descent into Evil, 13
  February 2021 - start of a blog series about deciding to use Evil, as an
  experienced Emacs user.

* https://github.com/tslilc/siege-mode - An emacs minor mode to surround the
  region with smart delimiters interactively.

2021-02-19
==========

* https://www.dr-qubit.org/Evil_cursor_model.html - Evil cursor model. The
  author thinks about how cursors work in emacs, vim and evil, and has
  suggestions for how things could be better (and provisional emacs lisp to
  make it so).

2021-02-23
==========

* https://github.com/grandfoobah/systemE - A lightweight systemd replacement
  written in Emacs lisp. Yes, it's a joke.

* https://github.com/skyler544/doom-nano-testing - an "attempt at adapting the
  gorgeous Nano Emacs by Nicolas Rougier to work with the powerhouse that is
  Doom Emacs."

  https://github.com/rougier/nano-emacs is an attempt to produce a version of
  Emacs based on design principles described in the article "On the design of
  text Editors" at https://arxiv.org/abs/2008.06030

  Probably worth looking at one day.

2021-03-01
==========

* https://linuxdatabases.info/blog/?p=267 - ielm – the interactive Emacs Lisp REPL

  Also see https://emacs-fu.blogspot.com/2011/03/ielm-repl-for-emacs.html

  and here is a video showing it in action: https://www.youtube.com/watch?v=lU1T2mqipN8

* https://github.com/bard/emacs-director - drives an Emacs session from the
  point of view of the user. It can be used for end-to-end testing, hands-free
  screencast recording, probably more.

  Generates a screencast using asciicinema (https://asciinema.org/,
  https://github.com/asciinema/asciinema)

  There's a nice preview on the page showing a pre-defined editing session
  playing back.

2021-03-02
==========

Tip of the day: ``M-x delete-trailing-whitespace`` will also remove stray
carriage returns (``^M``) if the file encoding is "unix" (``C-x RET f unix``).

2021-03-03
==========

* https://protesilaos.com/codelog/2021-03-03-emacs-query-replace-downcase/ -
  Use 'M-x query-replace-regexp' to downcase matches

  Neat use of ``query-replace-regexp`` with a callable.

    "For the replacement text I instructed the command to evaluate the
    downcase function. This is done by escaping the comma operator (``,``) and
    then supplying the function with a regexp group. Because I wished to match
    everything, the group should be ``\0``. Which means that the replacement
    should be expressed thus: ``\,(downcase \0)``"

2021-03-09
==========

* https://martin.baillie.id/wrote/evil-motion-training-for-emacs/ - a mode to
  make it harder to use "inefficient" evil motions - like all those Vim setups
  that stop you typing ``hhhhhhhh`` to get somewhere. It can even suggest
  better ways of doing things...

* https://github.com/sunnyhasija/Academic-Doom-Emacs-Config#theme - shows how
  to change the theme in Doom. This is hopeful because it suggests that one
  can use any Emacs theme, which is interesting and would allow me to explore
  more.

  (On the other hand, the theme I'm using, ``doom-one-light`` does seem quite
  nice)

  The preceding section shows how to specify fonts.

2021-03-15
==========

* http://mbork.pl/2021-03-15_Indenting_code_in_Emacs - Indenting code in
  Emacs. Extolls the virtues of ``C-x <TAB> (indent-rigidly)`` to allow
  (possibly interactive) adjustment of the indentation (of a region).

2021-03-23
==========

* https://github.com/kanru/uuidgen-el - UUID generation in emacs lisp


* https://endlessparentheses.com/emacs-advanced-basics-the-ins-and-outs-of-setf.html -
  the ins and outs of ``setf`` (a better way of setting things, originally
  from Common Lisp) - article from 2014

In the same "series" and around the same time:

* https://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html -
  Understanding letf and how it replaces flet
* https://endlessparentheses.com/emacs-advanced-basics-swaping-variables.html -
  Swaping Variables with cl-lib

2021-03-30
==========

* https://blog.lambda.cx/posts/emacs-align-columns/ - Aligning columns in
  Emacs with ``M-x align-non-space``

2021-04-06
==========

* https://tech.toryanderson.com/2021/04/03/selected-for-brilliant-emacs-selections/
  - a keymap for when the visual selection is active. It doesn't provide
  anyway default keys - purely up to the user to set them.

  https://github.com/Kungsgeten/selected.el

    "``selected.el`` provides the ``selected-minor-mode`` for Emacs. When
    ``selected-minor-mode`` is active, the keybindings in ``selected-keymap`` will be
    enabled when the region is active. This is useful for commands that
    operates on the region, which you only want bound to a key when the region
    is active. ``selected.el`` also provides ``selected-global-mode``, if you want
    ``selected-minor-mode`` in every buffer."

2021-04-13
==========

* https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
  - Speed up Emacs with libjansson and native elisp compilation

  libjansson is supported in Emacs 27, and makes JSON faster, which also makes
  lsp-mode or eglot faster.

  Native compilation (of elisp) is still  experimental, and probably won't be
  standard until Emacs 28, so some way off.

2021-04-23
==========

* https://irreal.org/blog/?p=9646 - Fifteen Days with Dired - links to the
  actual article at

  * https://www.ramitmittal.com/blog/15-days-in-dired/

  Note the "outer" article suggests "you can also map ``xdg-open`` or ``open``
  to the be the default action for the ``!`` and ``&`` commands" (``xdg-open``
  being the Linux equivalent of the Mac's ``open``).

  Strangely, neither of them mention ``wdired``. There's a nice article on
  that at https://masteringemacs.org/article/wdired-editable-dired-buffers,
  although of course the evil key bindings are different.

2021-05-04
==========

http://mbork.pl/2021-05-01_Emacs_Lisp_book_revived - Marcin Borkowski is
resuming work on his Intermediate Emacs Lisp book. It's being released as he
writes it at https://leanpub.com/hacking-your-way-emacs/ (as is normal for
LeanPub books).

He points out that for an *introduction* to programming with Emacs Lisp, one
should look to https://www.gnu.org/software/emacs/manual/eintr.html, which is
free.

--------

Coming back to https://tecosaur.github.io/emacs-config/config.html as a really
useful document on customising Doom.

2021-05-11
==========

https://ideas.offby1.net/posts/development-environment-2021.html

* using chezmoi_ to manage dotfiles and suchlike
* Doom emacs
* direnv_
* starship prompt
* asdf_ to "manage multiple runtime versions with a single CLI tool"

  (not the same as `Common Lisp ASDF`_, which is "the de facto standard build
  facility for Common Lisp")

.. _chezmoi: https://www.chezmoi.io/
.. _direnv: https://direnv.net/
.. _asdf: https://asdf-vm.com
.. _`Common Lisp ASDF`: https://common-lisp.net/project/asdf/

Also, interesting "readings" page from an AI programming course from Chris
Riesbeck at Northwestern University:

* https://courses.cs.northwestern.edu/325/index.php is the course summary
* https://courses.cs.northwestern.edu/325/readings/readings.php is the
  "readings" page.

Which references http://www.nhplace.com/kent/quoted.html#lisp-utility-caveat:

  “Please don't assume Lisp is only useful for Animation and Graphics, AI,
  Bioinformatics, B2B and Ecommerce, Data Mining, EDA/Semiconductor
  applications, Expert Systems, Finance, Intelligent Agents, Knowledge
  Management, Mechanical CAD, Modeling and Simulation, Natural Language,
  Optimization, Research, Risk Analysis, Scheduling, Telecom, and Web
  Authoring just because these are the only things they happened to list.”

  -- Kent Pitman, 2001

(I'm not sure if these are important enought to me to put into the Common Lisp
section proper.)

2021-06-03
==========

* https://github.com/tecosaur/emacs-everywhere - a pane of emacs for typing
  stuff that is to be inserted somewhere else

2021-06-25
==========

* https://christiantietze.de/posts/2021/06/emacs-trash-file-macos/ - make
  dired put deleted files into the (Mac OS) trash, instead of just deleting
  them.

2021-07-13
==========

* http://mbork.pl/2021-07-12_Counting_business_days - Counting business days
  in Emacs, including allowing for public holidays

* https://ruzkuku.com/texts/lesser-known.html - Some lesser known functions in
  Emacs. There's a summary post at https://irreal.org/blog/?p=9822

-----------

reStructuredText and rst.el
===========================

docutils has
https://sourceforge.net/p/docutils/code/HEAD/tree/trunk/docutils/tools/editors/emacs/rst.el

emacs has
https://github.com/emacs-mirror/emacs/blob/master/lisp/textmodes/rst.el

which is mirrored from git://git.sv.gnu.org/emacs.git (altahough I can't see that)

I *think* the version of the emacs lisp is 1.5.2, for emacs 26.2 and later,
but I should really check if those two files are the same.

Documentation (for v1.4.1 - ) is at
https://docutils.sourceforge.io/docs/user/emacs.html

There are quite a lot of FIXME style comments, and I do wonder if things like
the "temporary contents" generation wouldn't be better handled by leveraging
other existing tools.


Notes on using doom
===================

In no particular order:

* For windows, instead of CTRL-W <thing>, I can do SPACE w <thing>. This is
  so nice - it means no more CTRL-W !

  (I really need to learn to use SPACE as the leader key)

* For some reason, when typing reStructuredText in a list (like this) if I
  hit RETURN then it moves me to the left margin, not indented for the next
  line of the list. I assume I've got something somewhere misconfigured.

The following look like they have useful information:

* https://noelwelsh.com/posts/2019-01-10-doom-emacs.html - I think the "use
  the develop branch" is now out-of-date, but the hints on key sequences and
  common tasks look interesting/useful.

  At the end it says:

    I learned most of what I know about Doom by reading through the `default
    keybindings`_ and looking up commands I didn’t recognise. It’s easy to
    experiment in a scratch buffer to figure out what a command does.

  which sounds like sensible advice in several ways.

.. _`default keybindings`: https://github.com/hlissner/doom-emacs/
   blob/develop/modules/config/default/+evil-bindings.el

Videos to watch
===============
I have too many of these

* 2020-02-23 https://cestlaz.github.io/post/using-emacs-67-emacs-rant/ - Emacs
  vs Vi, a rant with some historical perspective
* 2020-02-07 https://www.youtube.com/watch?time_continue=1&v=VaBdlcYaZLQ&feature=emb_logo
  - Vim Versus Emacs. Which Is Better?

Packages I really should look at
================================

A never-ending list - but let's at least try to curate it a bit!

Here at the end because the end is easy to see...

These are deliberately not links - search elsewhere in this document for more
information (because sometimes there's contextual stuff near by).

* straight
* hyperbole
* projectile
* hydra
* live-py-plugin
* actually learn how to use magit

and

* sphinx-mode
* evil-commentary - commenting out lines
* dumb-jump - """an Emacs "jump to definition" package for 40+ languages""" -
  automated use of ``rg`` to try to jump to the definition of things.

and one of: helm or ivy

and one of:

* emacs-purpose - organise windows by "purpose"
* perspective-el - provides multiple workspaces (or "perspectives") for each
  Emacs frame
* or maybe just eyebrowse

Given how often I'm having to reboot my laptop (ick) I really should setup
some sort of session memory. Humph.

Not emacs, more stuff
=====================

fish on the far end of ssh
--------------------------

https://github.com/xxh/xxh-shell-fish - use fish on the other end of an ssh
link. Part of the https://github.com/xxh/xxh project.

Managing dotfiles
-----------------

https://github.com/deadc0de6/dotdrop is yet another way of managing your dot
files (using a git repository), but the example configuation file makes me
think this might be worth looking at.

(Although it's probably a bit overkill for what I want, and I might be better
borrowing the idea and just implementing what I actually want. For instance, I
think I still like the idea of a different repository for each "sort" of dot
file - it's particuarly nice to keep my Doom setup separate, because of this
file, for instance! On the other other hand, ``dotdrop`` has probably thought
of things I haven't...)

https://deadc0de.re/articles/dotfiles.html is their overview of the
alternatives and why they wrote their own solution.

https://dotfiles.github.io/ ("Your unofficial guide to dotfiles on GitHub") is
a good overview of what, why and how.

Programming languages
=====================

(other than emacs lisp)

I have a little list...
-----------------------

In no good order:

* Common Lisp (SBCL_ is obvious) - see `Common Lisp`_ below
* Rust_
* Elixir_ (on Erlang_, or more accurately on the BEAM)
* of course, Haxe_
* somehow I still want to play with Racket_ as well, even though I know I
  probably shouldn't...
* passerine_ looks very cute - I love that it uses ``--`` for comments (yes,
  that is a superficial thing to judge it on!)
* or maybe Gleam_ for a statically typed language on top of Erlang_

And see https://github.com/llaisdy/beam_languages for more languages on the
Erlang VM.

Common Lisp
-----------

* `Starting with Common Lisp in 2020`_ is (still) an interesting read (from
  January 2020). I've installed SBCL with::

    $ brew install sbcl

* https://lisp-journey.gitlab.io/blog/state-of-the-common-lisp-ecosystem-2020/
  (State of the Common Lisp ecosystem, 2020) is good, and was last updated
  Jan/Feb 2021

* https://ambrevar.xyz/modern-common-lisp/index.html (Modern, functional
  Common Lisp: Myths and tips) seems nice, and was last updated 2020-12-23

* https://stevelosh.com/blog/2018/08/a-road-to-common-lisp/ (A Road to Common
  Lisp, 2018) is nice (and opinionated). It suggests the following books, in
  the following order:

  * Common Lisp: A Gentle Introduction to Symbolic Computation

    This is available online, and as a PDF (both the 1990 edition). The print
    edition is 2013, reprint with some fixes. I tend to feel it starts rather
    too low level for me. He recommends it especially for the exercises -
    learn by doing.

  * `Practical Common Lisp`_. Got it - it looks delightful.

  * The actual language spec, for looking things up - also available online.
    http://www.lispworks.com/documentation/lw70/CLHS/Front/Contents.htm

  * Eventually, move on to learning idiomatic lisp - he recommends Paradigms
    of Artificial Intelligence Programming, also available as a free PDF.

  * Next, as a technical book, Common Lisp Recipes. Got it. It's good.

  * He also recommends (as a non-technical book) Patterns of Software by
    Richard Gabriel (available as a PDF)

  * After that:

    * for macros, first On Lisp and then Let Over Lambda (I have both) - take
      both with a pinch of salt (!)

    * for CLOS, Object-Oriented Programming in COMMON LISP: A Programmer's
      Guide to CLOS, and if one wants to bend one's mind, The Art of the
      Metaobject Protocol

  * He also has some affection for Land of Lisp

  Obviously The Common Lisp Condition System is too recent for an opinion to
  be there.

* https://lisp-lang.org/ and http://planet.lisp.org/

* https://lispcookbook.github.io/cl-cookbook/ - "The CL Cookbook aims to
  tackle all sort of topics, for the beginner as for the more advanced
  developer." Also downloadable in EPUB or PDF for a fee.

* and there is a github repository for https://github.com/norvig/paip-lisp
  (Paradigms of Artificial Intelligence Programming), which is a book that
  lots of people seem to like.

* And the actual standard (or the final draft thereof) is available at
  http://cberr.us/downloads/cl-ansi-standard-draft-w-sidebar.pdf

* https://github.com/lem-project/lem (Lem) is an editor written in Common
  Lisp. Technically it's an Emacs, I think (or like one) but clearly has vim
  bindings (of some sort) available. For what it's worth, its documentation
  could do with some love. But also, and refreshingly, it looks as if at least
  some of the core contributors are Japanese - the owner of the repository,
  https://github.com/cxxxr, who is also the main contributor, seems to be.

  But note https://github.com/lem-project/lem/tree/master/modes/vi-mode -
  there's already support for some sort of vim capability

* There's also https://github.com/robert-strandh/Second-Climacs, which appears
  to be active, but in very early stages. But it uses McClim, so it a GUI from
  the start (which might mean it won't do terminal mode).

* https://github.com/google/lisp-koans - Common Lisp koans, following the
  inspiration of the Ruby koans.

* https://github.com/CodyReichert/awesome-cl - A curated list of awesome
  Common Lisp libraries. And the related
  https://github.com/azzamsa/awesome-cl-software - A curated list of awesome
  application software built with Common Lisp.

* https://www.reddit.com/r/Common_Lisp/ has a list of useful resources down
  the right hand side (including many of the above)

and there are more articles linked on my pinboard.

Other Lisps and things
----------------------

Not forgetting Clojure_ or `Chicken Scheme`_ (meant to be good for CLI) or
even `Lisp Based Erlang`_. And, of course, there's Hy_ on the Python VM.

Even more Lisp links:

* https://cs.gmu.edu/~sean/lisp/LispTutorial.html - Lisp Quickstart, looks
  useful
* https://interlisp.org/ - Restore Interlisp-D to usability on modern
  OSes. Sounds interesting. Lisp + editing environment.

.. note:: And https://github.com/sasagawa888/eisl is an ISLisp compiler,
   apparently in active develoment, which is fun.

   * http://www.islisp.org/docs/islisp-v23.pdf is working draft 23 from 2007
   * https://islisp.js.org/ is an ISLisp on Go and/or Javascript
   * https://www.iso.org/standard/44338.html is the standards page - it sounds
     as if that 2007 draft is probably still current.

   Of course, I don't think ISLisp ever got formally "finished".


Proofy stuff
------------

Reading stuff by Hillel Wayne, for instance

* https://www.hillelwayne.com/post/formally-modeling-migrations/
* https://www.hillelwayne.com/post/using-formal-methods/
* https://www.hillelwayne.com/post/business-case-formal-methods/
* https://www.hillelwayne.com/post/why-dont-people-use-formal-methods/

makes both Alloy and TLA+ sound interesting, and actually approachable. See:

* https://www.quora.com/What-are-the-differences-between-Alloy-and-TLA+
* https://alloytools.org/tutorials/online/index.html
* https://learntla.com/introduction/ (introduction to Learn TLA+, by the
  aforesaid) and a review of the book:
  http://www.pathsensitive.com/2019/05/book-review-practical-tla.html; and a
  response to the review: https://old.reddit.com/r/tlaplus/comments/bptl02/book_review_practical_tla/

--------------

.. _Clojure: https://clojure.org/
.. _erlang: https://www.erlang.org/
.. _Gleam: https://gleam.run/
.. _Haxe: https://haxe.org/
.. _hy: https://github.com/hylang/hy
.. _Racket: https://racket-lang.org/
.. _rust: https://www.rust-lang.org/
.. _`Chicken Scheme`: https://call-cc.org/
.. _`Lisp Based Erlang`: http://lfe.io/
.. _`Practical Common Lisp`: http://www.gigamonkeys.com/book/
.. _`Starting with Common Lisp in 2020`: http://dnaeon.github.io/starting-with-common-lisp-in-2020/
.. _elixir: https://elixir-lang.org/
.. _passerine: https://www.passerine.io/
.. _sbcl: http://www.sbcl.org/

Common Lisp links
=================

Until I put them somewhere better...

2021-03-26
----------

* https://github.com/lokedhs/docbrowser - Web-based Common Lisp documentation
  browser (i.e., accesss common lisp docs in the local browser)
* https://lisp-journey.gitlab.io/blog/lem-can-be-started-as-a-full-featured-repl/
  - Lem can now be started as a full featured Lisp REPL

Other articles from that same blog:

* https://lisp-journey.gitlab.io/blog/slime-tips/ - with some links to other
  places as well.

And the author also writes in the Cookbook
https://lispcookbook.github.io/cl-cookbook/ (they say they're the main
contributor).

* https://github.com/willijar/cl-docutils - docutils in common lisp (so,
  reStructuredText support)

2021-03-30
==========

* https://github.com/mmontone/erudite/ - literate programming in Common Lisp.

2021-04-08
==========

* https://yitzchak.github.io/common-lisp-jupyter/ - Common Lisp for Jupyter
  notebooks.
