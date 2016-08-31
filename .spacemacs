;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     ;; better-defaults
     emacs-lisp
     git
     ;; markdown
     org
     latex
     (haskell :variables
              haskell-enable-ghci-ng-support t
              haskell-enable-shm-support t
              haskell-enable-hindent-style "chris-done")
     agda
     themes-megapack
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     (spell-checking :variables spell-checking-enable-by-default nil)
     syntax-checking
     version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(rainbow-mode)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   ;;   (This was the list that shipped default)
   ;;   dotspacemacs-themes '(spacemacs-dark
   ;;                    spacemacs-light
   ;;                    solarized-light
   ;;                    solarized-dark
   ;;                    leuven
   ;;                    monokai
   ;;                    zenburn)

   dotspacemacs-themes '(bkava-light
                         solarized-light
                         moe-light
                         meacupla
                         leuven


  adwaita
                         mccarthy
                         professional
                         ritchie
                         sunny-day
                         soft-stone

                         tango)

   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.

   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."
  ;; tell haskell-mode to use "stack ghci" to run interpreter
  (setq haskell-process-type 'stack-ghci)

  ;; experiment with these minor modes to see which I want to use
  ;; 
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  ;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode) This is added by default haskell layer
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

 )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  ;; This code was to fix evil-jumper (C-i) behaviour. I believe this was fixed with 
  ;; change to distinguish-gui-tab  config param to be true.
  ;; (require 'evil-jumper)
  ;; (evil-jumper-mode t) 
  ;; (setq evil-want-C-i-jump t)
  ;; (define-key input-decode-map (kbd "C-i") (kbd "H-i"))
  ;; (global-set-key (kbd "H-i") 'evil-jump-forward)

  ;; add pdflatex command to tex compile menu
  (defcustom custom-TeX-command-list
    ;; '(("pdflatex" "pdflatex '\\nonstopmode\\input{%t}'" TeX-run-LaTeX nil t))
    nil
    "Additional Tex commands")
  ;; ok. now that this works, test with a latex mode hook
  ;; (add-hook 'LaTeX-mode-hook
  ;;           (lambda () ...
  ;;                      (setq TeX-command-default "latexmk")
  (eval-after-load "tex"
    '(setq TeX-command-list
           (append custom-TeX-command-list
                   TeX-command-list)))

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; WIKI STUFF. muse, remember, planner
  ;; muse stuff

  (require 'muse-mode)     ; load authoring mode

  (require 'muse-html)     ; load publishing styles I use
  (require 'muse-latex)
  (require 'muse-texinfo)
  (require 'muse-docbook)
  (require 'muse-blosxom)

  (require 'muse-project)  ; publish files in projects

  (setq muse-dir "~/archy/")
  (defun in-muse-dir (suffix) (concat muse-dir suffix))

  (setq muse-project-alist
        ;; General Wiki (general knowledge base, mostly about stuff related to personal pursuits (sailing, yoga, etc)
        `(("Wiki" (,@(muse-project-alist-dirs (in-muse-dir "wiki"))
                   :default "IndexWiki")
           ,@(muse-project-alist-styles (in-muse-dir "wiki")
                                        "~/public_html/wiki"
                                        "xhtml"))
          ;; Research Wiki (pages about programming, logic, mathematics, etc)
          ("Research" (,@(muse-project-alist-dirs (in-muse-dir "research"))
                       :default "IndexResearch")
           ,@(muse-project-alist-styles (in-muse-dir "research")
                                        "~/public_html/research"
                                        "html"))

          ;; Bibliography. Perhaps I should just use citeulike or a built in citation manager. I think writing an 
          ;; explicit manager would be better than this. but it works for now. 
          ("Library" (,@(muse-project-alist-dirs (in-muse-dir "library"))
                      :default "IndexLib")
           ,@(muse-project-alist-styles (in-muse-dir "library")
                                        "~/public_html/library"
                                        "html"))

          ;; Projects area. This is where I describe all of my projects in text. from here I move tasks to staging 
          ;; area in work and personal.org. from there they go -> todo.org. I work from todo.org
          ("Work" (,@(muse-project-alist-dirs (in-muse-dir "projects/work"))
                   :default "IndexWork")
           ,@(muse-project-alist-styles (in-muse-dir "projects/work")
                                        "~/public_html/projects/work"
                                        "html")
           )

          ;; Projects area. This is where I describe all of my projects in text. from here I move tasks to staging
          ;; area in work and personal.org. from there they go -> todo.org. I work from todo.org
          ("Life" (,@(muse-project-alist-dirs (in-muse-dir "projects/life"))
                   :default "IndexLife")
           ,@(muse-project-alist-styles (in-muse-dir "projects/life")
                                        "~/public_html/projects/life"
                                        "html")
           )

          ;; Log. This is where I put all of my daily notes, completed tasks, timelog. Important writing goes to Wiki or Research.
          ("Log" (,@(muse-project-alist-dirs (in-muse-dir "logs"))
                  :default "IndexLog")
           ,@(muse-project-alist-styles (in-muse-dir "logs")
                                        "~/public_html/logs"
                                        "html"))))


  (setq log-project "Log")

  ;; remember. I should use capture from org-mode and move remember templates to org-capture templates.
  (require 'remember)


  ;; unused planner stuff.
  ;; (global-set-key (kbd "<f9> t") 'planner-create-task-from-buffer)
  ;; (global-set-key (kbd "<f9> r") 'remember)
  ;; (global-set-key (kbd "<f9> g") 'planner-goto-today)



  ;; this sets muse mode by file location.
  (setq muse-file-extension nil
        muse-mode-auto-p t)

  ;;   I may need to add this if .emacs ever reads a muse file and requires it to be in musemode.
  ;;
  (add-hook 'find-file-hooks 'muse-mode-maybe)

  ;;;; END MUSE

  ;; modify browse-url to use chrome.
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome")

  ;;; Agda stuff  (also some custom variables in variables section)

  ;;  (load-file (let ((coding-system-for-read 'utf-8))
  ;;             (shell-command-to-string "agda-mode locate")))

  (custom-set-variables
   '(agda-input-user-translations (quote (("swap" "⇆") ("nms" "𝔸") ("bool" "𝔹") ("_rho" "ᵨ") ("^e" "ᵉ") ("_beta" "ᵦ") ("_i" "ᵢ") ("_n" "ₙ"))))
   '(agda2-include-dirs (quote ("." "/usr/local/share/agda-stdlib-0.11")))
   '(auto-compression-mode t nil (jka-compr))
   ;; '(case-fold-search t)
   '(current-language-environment "UTF-8")
   '(default-input-method "rfc1345")
   '(display-time-mode t)
   '(gds-search-url "http://127.0.0.1:39401/search?q=")
   '(global-font-lock-mode t nil (font-lock))
   '(org-agenda-custom-commands (quote (("c" todo "DONE|DEFERRED|CANCELLED" nil) ("w" todo "WAITING" nil) ("W" agenda "" ((org-agenda-ndays 21))) ("A" agenda "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]"))) (org-agenda-ndays 1) (org-agenda-overriding-header "Today's Priority #A tasks: "))) ("u" alltodo "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline) (quote regexp) "<[^>
]+>"))) (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
   '(org-agenda-files (quote ("~/archy/org/main.org ~/writing/thesis/thesis.org")))
   '(org-agenda-ndays 7)
   '(org-agenda-show-all-dates t)
   '(org-agenda-skip-deadline-if-done t)
   '(org-agenda-skip-scheduled-if-done t)
   '(org-agenda-start-on-weekday nil)
   '(org-deadline-warning-days 14)
   '(org-default-notes-file "~/archy/org/notes.org")
   '(org-fast-tag-selection-single-key (quote expert))
   '(org-remember-store-without-prompt t)
   '(org-remember-templates (quote ((99 "** %U %?
  %a
  " "~/archy/org/calculo.org" "Notes") (108 "** %U %?" "~/archy/org/daily.org" "Timelog") (110 "** %u %?" "~/archy/org/notes.org" "Notes") (116 "* TODO %?
  %u" "~/archy/org/todo.org" "Tasks"))))
   '(org-reverse-note-order t)
   '(quack-programs (quote ("mred " "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
   '(remember-annotation-functions (quote (org-remember-annotation)))
   '(remember-handler-functions (quote (org-remember-handler)))
   '(show-paren-mode t nil (paren))
   '(tool-bar-mode nil))

  ;;  Hmm .. I should really put this in bkava theme. 
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(highlight-numbers-number ((t (:inherit font-lock-constant-face :foreground "green4")))))

)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda-input-user-translations
   (quote
    (("swap" "⇆")
     ("nms" "𝔸")
     ("bool" "𝔹")
     ("_rho" "ᵨ")
     ("^e" "ᵉ")
     ("_beta" "ᵦ")
     ("_i" "ᵢ")
     ("_n" "ₙ"))))
 '(agda2-include-dirs (quote ("." "/usr/local/share/agda-stdlib-0.11")))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(auto-compression-mode t nil (jka-compr))
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(display-time-mode t)
 '(fci-rule-color "#eee8d5" t)
 '(gds-search-url "http://127.0.0.1:39401/search?q=")
 '(global-font-lock-mode t nil (font-lock))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-sexp-background-color "#efebe9")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-custom-commands
   (quote
    (("c" todo "DONE|DEFERRED|CANCELLED" nil)
     ("w" todo "WAITING" nil)
     ("W" agenda ""
      ((org-agenda-ndays 21)))
     ("A" agenda ""
      ((org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#A\\]")))
       (org-agenda-ndays 1)
       (org-agenda-overriding-header "Today's Priority #A tasks: ")))
     ("u" alltodo ""
      ((org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote scheduled)
           (quote deadline)
           (quote regexp)
           "<[^>
]+>")))
       (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files
   (quote
    ("~/archy/org/main.org ~/writing/thesis/thesis.org")))
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/archy/org/notes.org")
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote
    ((99 "** %U %?
  %a
  " "~/archy/org/calculo.org" "Notes")
     (108 "** %U %?" "~/archy/org/daily.org" "Timelog")
     (110 "** %u %?" "~/archy/org/notes.org" "Notes")
     (116 "* TODO %?
  %u" "~/archy/org/todo.org" "Tasks"))))
 '(org-reverse-note-order t)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(quack-programs
   (quote
    ("mred " "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(show-paren-mode t nil (paren))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#fdfde7" :foreground "#5f5f5f" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "unknown" :family "InconsolataGG"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(highlight-numbers-number ((t (:inherit font-lock-constant-face :foreground "green4")))))
