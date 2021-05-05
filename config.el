;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tibs"
      user-mail-address "tibs@tonyibbs.co.uk")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;(setq doom-theme 'doom-one-light)
(setq doom-theme 'doom-one-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
;; When presented with duplicate buffer names, I'd like a name rather than
;; a number to distinguish them. For instance:
;;
;;   buffer.rb <this_dir>
;;
;; rather than
;;
;;   buffer.rb <2>
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; I was finding that `doom doctor` would report that my projectile cache
;; was too large. This can be "solved" by deleting the file, which lives
;; at ~/.emacs.d/.local/cache/projectile.cache
;;
;; However, it may also be worth using "alien" indexing, and avoiding the use
;; of a cache. Whilst the projectile documentation says this is the default
;; from projectile 2.0, it seems to be set to "hybrid" for me at the moment
;; (Doom v2.0.9, 2021-05-05)
;;
;; See https://docs.projectile.mx/projectile/configuration.html#project-indexing-method
(setq projectile-indexing-method 'alien)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; -----------------------------------------------------------------------------
;; Experimental configuration from my original emacs/evil setup

;; Let's default to a sensible-ish auto-fill column
(setq-default fill-column 78)

;; Change the default frame size to something more suitable
;; (set the default width to 121 so that the 120th character doesn't cause
;; soft line wrap, from visual-line-mode, to trigger)
(add-to-list 'default-frame-alist '(width  . 121))
(add-to-list 'default-frame-alist '(height . 75))

;; Since I'm (probably) using a vanilla-ish Emacs (rather than, say, Aquamacs),
;; the default binding of Meta is to <Opt ⌥>, which Emacs regards as <Alt>.
;; That's a problem if I want to type <hash> or <euro>, which are <Opt-3> and
;; <Opt-2> respectively.
;;
;; Reminder:
;;
;;    Esc    Escape              use by Evil (!)
;;    Alt    Option,  <Opt ⌥>    default binding for Meta
;;    Cmd    Command, <Cmd ⌘>
;;
;; and available settings are 'control, 'meta, 'alt, 'super and 'hyper
;;
;; The "super" key is the Windows key, or on a Mac the Command key.
;; The "alt" key is the key that says Alt (surprise). It is normally chosen as the
;; Meta key (on Windows and on Linux).
;; https://en.wikipedia.org/wiki/Super_key_(keyboard_button) is quite a nice summary
;; with some useful historical context.

;; For reference, this is what Opt-<digit> seems to give for each of 0..9:
;;
;;    1234567890
;;    ¡€#¢∞§¶•ªº

;; Ideally I'd probably conditionalise this on whether I'm on a Mac, and even
;; dependent on which keyboard I'm using. But for the moment, let's just get
;; stuff working
;;
;; The approach that works is as described at the end of the article at
;; follow https://emacs.stackexchange.com/questions/26390/passing-altletter-keybindings-through-to-osx
;;
;; (using the initial suggestion there, of doing
;;      (global-set-key (kbd "\M-3") (lambda () "Insert #." (interactive) (insert "#")))
;; doesn't work, as in insert "mode" it it results in M-3 bound to digit-argument - i.e., it's treating
;; it as a numeric argument for the next command)

;; Note that the `(meta ?x)` needs to be wrapped as a list in order to allow the
;; `meta` name to be used.
(define-key key-translation-map [(meta ?2)] [?€])
(define-key key-translation-map [(meta ?3)] [?#])

;; I should perhaps use map! for those - see
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/api.org
;; and in particular the section starting "These are side-by-side comparisons,
;; showing how to bind keys with and without map!", for how to bind keys with
;; map!

;; I want ``_'' to be treated as a word character, by default
;; The Doom FAQ says I just need to do:
(modify-syntax-entry ?_ "w")

;; In my Vim setup, I was used to using "%/" in the ex line to expand out
;; to the directory of the current buffer. This sort of thing turns out to be
;; possible in Emacs/Evil as well, although it too me a while to work it out.
;;
;; The following function is based on insert-current-file-name-at-point, from
;; http://mbork.pl/2019-02-17_Inserting_the_current_file_name_at_point
(defun insert-current-directory-at-point ()
  "Insert the current directory at point."
  (interactive)
  (let* ((buffer
          (if (minibufferp)
              (window-buffer
               (minibuffer-selected-window))
            (current-buffer)))
         (filename (buffer-file-name buffer)))
    (if filename
        (insert (file-name-directory filename))
      (error (format "Buffer %s is not visiting a file" (buffer-name buffer))))
    ))
;; For various reasons, both in Vim and in Emacs I now bind this to "@ /" instead
;; Doom makes this quite easy
(map! :map evil-ex-completion-map
      "@ /" #'insert-current-directory-at-point)

;; Fiddle with the name used for this window, and for when we
;; are iconized The format content is defined in the
;; documentation for `modeline-format'
;; - in particular:
;;      %b means the current buffer name,
;;      %f means the visited file name
;; (setq frame-title-format '("%b - %f"))
(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b"))
      )

;; Packages
;; ========

;; reStructuredText
(after! rst
  ;; The default indentation is 3, which I'm not so keen on
  ;; (although I'm happy for it to be 3 for comments)
  (setq rst-indent-literal-normal 2))

;; and I probably need to set evil-shift-width as well, to make indenting
;; a region work as expected
(add-hook! rst-mode
  (setq evil-shift-width 2))

;; For the above, it's old (2011) but see
;;   https://stackoverflow.com/questions/8473131/set-the-evil-shift-width-to-the-buffer-local-indentation-in-emacs
;; and not so old, but even more useful
;;   https://dougie.io/emacs/indentation/
;; which also shows setting evil-shift-width, and for what sounds like the
;; same reason

;; Sometimes, electric-indent-mode doesn't play well with other things
;(add-hook! rst-mode (electric-indent-local-mode -1))
;; Actually, that didn't seem to be the problem

(setq auto-mode-alist
      (append '(("\\.txt\\'" . rst-mode)
                ("\\.rst\\'" . rst-mode)
                ("\\.rest\\'" . rst-mode)) auto-mode-alist))


;; Make reStructuredText mode the default (instead of Fundamental)
;(defvar default-major-mode)
(setq default-major-mode  'rst-mode)

;; Since rst-mode calls text-mode-hook and rst-mode-hook, we can generalise our
;; use of default auto-fill to both
(add-hook! text-mode 'turn-on-auto-fill)
;(add-hook! text-mode (lambda ()
;                            (fci-mode)  ; fill-column-indicator
;                            ))

;; Make the Emacs kill ring and the system clipboard independent
;; The package documentation says:
;;
;;   Press super-c to copy without affecting the kill ring.
;;   Press super-x or super-v to cut or paste.
;;   On OS X, use ⌘-c, ⌘-v, ⌘-x.
(simpleclip-mode 1)

;; I *think* that using 2 as a general indentation works better
;; - it's what I want in rsStructuredText, and in Ruby (but not Python,
;; so we'll deal with that separately)
;(setq-default evil-shift-width 2)

;; When I'm writing Ruby code, I don't want emacs to put a magic comment:
;;   # coding: utf-8
;; at the top of any of my files, because UTF-8 is now the default for Ruby
;; source code files (.rb files). Hopefully this should stop that from happening
(setq ruby-insert-encoding-magic-comment nil)

;(add-hook 'python-mode-hook (lambda ()
;                              (fci-mode)  ; fill-column-indicator
;                              ))

(setq-default python-indent-offfset 4)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;(use-package cargo
;  :ensure t
;  :hook (rust-mode . cargo-minor-mode))

;; Use spaces for indentation
;(add-hook 'rust-mode-hook
;          (lambda () (setq indent-tabs-mode nil)))

;; Other stuff

;; Remember where I was in a file, so emacs will go back when I re-open it
(save-place-mode 1)

;; Make sure the autosaves directory exists, as emacs won't create it
;; (but will get upset if it doesn't exist)
;(make-directory (concat user-emacs-directory "autosaves/") t)
;
;; That defaults to saving the place location in a file called ~/.emacs.d/places
;; I can change that by doing:
;;   (setq save-place-file (locate-user-emacs-file "places" ".emacs-places"))
;; If exiting emacs is very slow (historically, when files are on NFS) then it
;; might be helpful to stop an extra check on exit:
;;   (setq save-place-forget-unreadable-files nil)

;; Keep my autosave files in one place, rather than "next to" the file being edited
;(setq backup-directory-alist
;      `(("." . ,(concat user-emacs-directory "backups"))))
;(setq auto-save-file-name-transforms
;      `((".*" ,(concat user-emacs-directory "autosaves/") t)))

;; And ask it to use a simple numbering scheme on its backup files (the ones ending '~')
;; (I'm actually ambivalent about this, but let's try it for a little while)
;; See (2008/2013ish) https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;(setq version-control t
;      kept-new-versions 2       ; the default
;      kept-old-versions 0       ; the default is 2
;      delete-old-versions t     ; silently delete backups when a file is saved
;      )

;; Not sure I want to disable the mouse, really, depending on how Doom works with it
;; (require 'disable-mouse)
;; And the disable-mouse home page (https://github.com/purcell/disable-mouse)
;; recommends that Evil users also do the following. Note that turning OFF
;; disable-mouse-mode won't undo these - they can only be undone by restarting
;; Emacs (without them in my init.el!)
;(mapc #'disable-mouse-in-keymap
;      (list evil-motion-state-map
;            evil-normal-state-map
;            evil-visual-state-map
;            evil-insert-state-map))

;; I don't use the tool bar at the top of each window.
;; This disables it in GUI mode
;(tool-bar-mode -1)

;; In terminal mode, I also want to disable the menu bar
;(when (not (display-graphic-p))
;  (menu-bar-mode -1))

;; CRUDELY default to only using spaces for indentation, not tabs
;; Experimentation shows that Makfiles are special enough to ignore this
;; (which is good)
;(setq-default indent-tabs-mode nil)

;; Enable the following if I want to always use y or n instead of yes or no
;; (defalias 'yes-or-no-p 'y-or-n-p)

;; When starting up with a file, I don't want to see the splash screen as well.
;; It's easy to just *never* show the splash screen:
;(setq inhibit-startup-screen t)
;; but there's no *obvious* way to only show the splash screen if no files were
;; specified. On the other hand, that's not something I do often, and C-h C-a
;; can always be used to see the splash screen if one wants (and it *does* tell
;; you that when Emacs starts up)

;; Highlight trailing whitespace in red, so it’s easily visible
(customize-set-variable 'show-trailing-whitespace t)

;; More control over how whitespace is shown
;; This is something of an experiment.
(setq whitespace-style '(face trailing tabs lines-tail indentation::space space-before-tab tab-mark))

;; Change how empty line markers are handled
;(setq-default indicate-empty-lines t)
;(when (not indicate-empty-lines)
;  (toggle-indicate-empty-lines))

;; Q q doesn't seem to exist for paragraph filling
;; (I think in vim I got it by doing ``map Q gq``?)
;; Borrow the following from aquamacs (it's fairly obvious):
(defun fill-paragraph-or-region (&optional justify)
  "Fill paragraph or region (if any).
When no region is defined (mark is not active) or
`transient-mark-mode' is off, call `fill-paragraph'.
Otherwise, call `fill-region'.
If `word-wrap' is on, and `auto-fill-mode off, call
`unfill-paragraph-or-region' instead."
  (interactive "P")
  (if (and word-wrap (not auto-fill-function))
      (call-interactively 'unfill-paragraph-or-region)
    (if (and mark-active transient-mark-mode)
        (call-interactively 'fill-region)
      (call-interactively 'fill-paragraph))))

;; and just define the key sequence we want
(map! :map evil-normal-state-map
      "Q q" #'fill-paragraph-or-region)
;; I used to do
;; (define-key evil-normal-state-map (kbd "Q q") 'fill-paragraph-or-region)


;; NB: see https://github.com/hlissner/doom-emacs/blob/develop/docs/api.org
;; and in particular the section starting "These are side-by-side comparisons,
;; showing how to bind keys with and without map!", for how to bind keys

;; Note that the default paragraph fill in Emacs appears to operate rather
;; later than in Vim - i.e., when a whole word has occurred past the fill
;; column, not when a word crosses it. I *think* I prefer the Vim approach.

;; When lines *are* too long to show in the window, then make them show as
;; wrapped
(global-visual-line-mode)

;(use-package evil-numbers
;  :ensure t)
;; evil-numbers provides the Vim capability to increment/decrement a number
;; occurring in the (following) text. However, the operations can't be bound
;; to C-a and C-x because C-x is *very strongly* a prefix key in emacs (i.e.,
;; it's always C-x <something>). So we have to choose something else.
;; For the moment I'm going for C-c = (remember, = is the unshifted +)
;; and C-c - (control minus). Neither is bound to anything standard
;(define-key evil-normal-state-map (kbd "C-c =") 'evil-numbers/inc-at-pt)
;(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; Enable epub support
;; NB: requires an 'unzip' binary on the PATH, and libxml2 support in Emacs
;(use-package nov
;  :ensure t)
;(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; From https://irreal.org/blog/?p=9482 (Fixing Two Spaces Between Sentences),
;; how to tell Emacs that I actually want a *single* space after the end of
;; a sentence, instead of two.
(setq sentence-end-double-space nil) ; <period> <single space> ends sentence

;; ===============================================================
;;; Modeline

;; Let's try a powerline - they seem nice

;; I rather like telephone-line. It supports evil out-of-the-box
;;    https://github.com/dbordak/telephone-line
;; The default seems quite nice for common use. I like the fact that it
;; shows columns by default, and that it indicates when flycheck gives
;; errors (and clicking on the indicator for that will open a buffer
;; showing details, which is nice).
;; It *does* seem to give some warnings when byte-compiling.
;; Any customisation of the telephone-line needs to go here.
;(use-package telephone-line
;  :ensure t
;  :config
;  (telephone-line-mode 1)
;  )

;; Display the column number in the mode line
;; (setq column-number-mode t)

;; Highlight the current line.
;; Some evil setups do this automatically, and I think it is worth experimenting
;; with, not least because it also helps make it clear which frame/window/buffer
;; is actually active/selected. Interestingly, it also seems to help me visualise
;; how many spaces there are in the left gutter (next to the current line number),
;; important when I'm deleting automatically inserted indentation.
;; The default appears to be to highlight the current line with a light green
;; background.
;(global-hl-line-mode 1)

;; Miscellanea
;; ===========
;; Always enable the (vertical) scrollbar
(scroll-bar-mode)

;; ===============================================================
;; Support for the log files I write at work.
;; Improvements to consider:
;;
;; * Allow the directory to be specified.
;; * Allow the date to be specified.
;; * Allow the "Logbook for" string to be specified

;; Taken from https://stackoverflow.com/a/20317537
(defun ordinal (n)
  "Represent the integer N as an ordinal string."
  (format
   (concat
    "%d"
    (if (memq n '(11 12 13)) "th"
      (let ((last-digit (% n 10)))
        (cl-case last-digit
          (1 "st")
          (2 "nd")
          (3 "rd")
          (otherwise "th"))))) n))


;; Taken from https://stackoverflow.com/a/20317537
(defadvice format-time-string (before ordinal activate)
  "Implement the %o format, which is an ordinal form of %d."
  (let ((day (nth 3 (decode-time (or time (current-time))))))
    (setq format-string
      (replace-regexp-in-string "%o"
                    (ordinal day)
                    format-string))))

(defun insert-datestamp ()
  "Insert a datestamp title, based on todays date.

  For instance:

   2019-09-23
   ==========

   Monday 23rd September 2019

  Leaves point on the second blank line after the title."
  (interactive)
  (insert
   (concat
    (format-time-string "%Y-%m-%d\n")
    "==========\n"
    "\n"
    (format-time-string "%A %o %B %Y\n\n"))))

(defun insert-logbook-title ()
  "Insert a logbook title, based on todays date.

  For instance:

   ======================
   Logbook for 2019-09-23
   ======================

   Monday 23rd September 2019

  Leaves point on the second blank line after the title."
  (interactive)
  (insert
   (concat
    "======================\n"
    (format-time-string "Logbook for %Y-%m-%d\n")
    "======================\n"
    "\n"
    (format-time-string "%A %o %B %Y\n\n"))))

(defun open-logfile ()
  "Open (creating if necessary) a logfile with todays date.

  Inserts a logbook title block if the buffer is empty (whether the file
  existed or not)."
  (interactive)
  (find-file (format-time-string "~/Documents/notes/logbook/logbook-%Y%m%d.txt"))
  (if (= (buffer-size) 0)
      (insert-logbook-title)))

(defun yesterday ()
  "Open yesterday's logfile.

  This is so not the best way to do it.

  Ideally I probably want something (a) with a better title and (b) that
  can do things like:

  * open yesterday
  * open last friday

  ...actually, that's probably all the sorts of thing I want..."
  (interactive)
  (let* ((now (current-time))
         (yesterday (time-add now (* -24 3600))))
    (find-file (format-time-string "~/Documents/notes/logbook/logbook-%Y%m%d.txt" yesterday))))

;; ===============================================================
;;; LEGACY IDEAS (from old emacs init files)

;; Make dates in calendar/diary mode be European
(defvar european-calendar-style)
(setq european-calendar-style t)

;; When editing C code (and Lisp code and the like), allow tabs
;; to be inserted into comments and so on without having to use
;; C-q TAB".  Note that this does not work in TTY frames, where
;; tab and shift-tab are indistinguishable.
;;(define-key global-map '(shift tab) 'self-insert-command)

;; Always check whether we should end a file with a newline
;; (this appears to be a pain for the `autosave` file...'')
;(setq require-final-newline (quote ask))
