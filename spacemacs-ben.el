;;  Put all initialisation customizations here.
;;  modularize into functions. 
(defun spacemacs-ben/global ()
  ;; fonts
  ;; this prob not necessary as font-lock is default
  (global-font-lock-mode t)
  ;; Turn on editing compressed files (modern defaults should be ok)
  (auto-compression-mode)

  (setq quack-programs
        '("mred " "bigloo" "csi" "csi -hygienic"
          "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile"
          "kawa" "mit-scheme" "mred -z" "mzscheme"
          "mzscheme -M errortrace" "rs" "scheme" "scheme48"
          "scsh" "sisc" "stklos" "sxi"))

  ;; if necessary can make show-paren-mode local
  (show-paren-mode 1)
  (tool-bar-mode -1)
  (display-time-mode 1)
  )

(defun spacemacs-ben/browse ()
  ;; modify browse-url to use chrome.
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome")
  )


(defun spacemacs-ben/language-input ()
  ;; Setting current language and input
  (setq-default current-language-environment "UTF-8")
  (setq default-input-method "rfc1345")
  )


(defun spacemacs-ben/haskell ()
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


(defun spacemacs-ben/fix-jumper ()
  ;; This code was to fix evil-jumper (C-i) behaviour. I believe this was fixed with 
  ;; change to distinguish-gui-tab  config param to be true.
  (require 'evil-jumper)
  (evil-jumper-mode t) 
  (setq evil-want-C-i-jump t)
  (define-key input-decode-map (kbd "C-i") (kbd "H-i"))
  (global-set-key (kbd "H-i") 'evil-jump-forward)
  )


(defun spacemacs-ben/tex ()

  ;; add pdflatex command to tex compile menu
  (defcustom custom-TeX-command-list
    ;; '(("pdflatex" "pdflatex '\\nonstopmode\\input{%t}'" TeX-run-LaTeX nil t))
    nil
    "Additional Tex commands")

  (eval-after-load "tex"
    '(setq TeX-command-list
           (append custom-TeX-command-list
                   TeX-command-list))))

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; WIKI STUFF. muse, remember, planner
  ;; muse stuff

(defun spacemacs-ben/muse ()

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
  ;; this sets muse mode by file location.
  (setq muse-file-extension nil
        muse-mode-auto-p t)

  ;;   I may need to add this if .emacs ever reads a muse file and requires it to be in musemode.
  ;;
  (add-hook 'find-file-hooks 'muse-mode-maybe)
  ;;;; END MUSE
  )


(defun spacemacs-ben/plan-remember ()
  ;; remember. I should use capture from org-mode and move remember templates to org-capture templates.
  (require 'remember)

  (setq-default remember-annotation-functions (quote (org-remember-annotation)))
  (setq remember-handler-functions (quote (org-remember-handler)))

  ;; unused planner stuff.
  ;; (global-set-key (kbd "<f9> t") 'planner-create-task-from-buffer)
  ;; (global-set-key (kbd "<f9> r") 'remember)
  ;; (global-set-key (kbd "<f9> g") 'planner-goto-today)
  )



(defun spacemacs-ben/agda ()
  ;;; Agda stuff  (also some custom variables in variables section)

  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))

  (setq-default agda-input-user-translations
                '(("swap" "⇆")
                  ("nms" "𝔸")
                  ("bool" "𝔹")
                  ("_rho" "ᵨ")
                  ("^e" "ᵉ")
                  ("_beta" "ᵦ")
                  ("_i" "ᵢ")
                  ("_n" "ₙ")))

  (setq-default agda2-include-dirs (quote ("." "/usr/local/share/agda-stdlib-0.11")))
  )


(defun spacemacs-ben/org ()
  (setq org-agenda-custom-commands
        '(("c" todo "DONE|DEFERRED|CANCELLED" nil)
          ("w" todo "WAITING" nil)
          ("W" agenda "" ((org-agenda-ndays 21)))
          ("A" agenda "" ((org-agenda-skip-function
                           (lambda nil
                             (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
                          (org-agenda-ndays 1)
                          (org-agenda-overriding-header "Today's Priority #A tasks: ")))
          ("u" alltodo "" ((org-agenda-skip-function
                            (lambda nil
                              (org-agenda-skip-entry-if
                               'scheduled 'deadline 'regexp "<[^>]+>")))
                           (org-agenda-overriding-header "Unscheduled TODO entries: ")))))

  (setq org-agenda-files '("~/archy/org/main.org ~/writing/thesis/thesis.org"))
  (setq org-agenda-ndays 7)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-deadline-warning-days 14)
  (setq org-default-notes-file "~/archy/org/notes.org")
  (setq org-fast-tag-selection-single-key (quote expert))
  (setq org-remember-store-without-prompt t)
  (setq org-remember-templates
        '((99 "** %U %? %a" "~/archy/org/calculo.org" "Notes")
          (108 "** %U %?" "~/archy/org/daily.org" "Timelog")
          (110 "** %u %?" "~/archy/org/notes.org" "Notes")
          (116 "* TODO %? %u" "~/archy/org/todo.org" "Tasks")))
  (setq org-reverse-note-order t)

 ) 


(defun spacemacs-ben/bkava-fix ()
  ;;  Hmm .. I should really put this in bkava theme. 
  (set-face-attribute 'highlight-numbers-number t :inherit font-lock-constant-face :foreground "green4")
  )


(defun spacemacs-ben/init()
  ;; general config
  (spacemacs-ben/global)
  (spacemacs-ben/language-input)
  (spacemacs-ben/browse)
  ;; programming 
  (spacemacs-ben/haskell)
  (spacemacs-ben/agda)
  (spacemacs-ben/tex)
  ;; emacs organisation/notes
  (spacemacs-ben/org)
  (spacemacs-ben/muse)
  (spacemacs-ben/plan-remember)

  ) 
