;; -*- lexical-binding: t -*-
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(eval-when-compile
  (require 'use-package))

;; Core
(use-package use-package
  :config
  (setq use-package-always-ensure t
	use-package-expand-minimally t)
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package"))
  (require 'vc-use-package))

(use-package emacs
  :ensure nil
  :init
  (defun insert-line-above ()
    "Insert and indent a new line above the cursor."
    (interactive)
    (forward-line -1)
    (move-end-of-line 1)
    (newline-and-indent))

  (defun insert-line-below ()
    "Insert and indent a new line below the cursor."
    (interactive)
    (move-end-of-line 1)
    (newline-and-indent))
  
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; File protection
  (defvar backups-dir (concat user-emacs-directory "backups/"))
  (defvar autosaves-dir (concat user-emacs-directory "autosaves/"))
  (defvar locks-dir (concat user-emacs-directory "locks/"))

  (setq backup-directory-alist
	`(("." . ,backups-dir)))
  (setq auto-save-file-name-transforms
	`((".*" ,autosaves-dir t)))
  (setq lock-file-name-transforms
	`((".*" ,locks-dir t)))
  
  (dolist (dir (list user-emacs-directory
		     backups-dir autosaves-dir locks-dir))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  
  (add-to-list 'default-frame-alist
               '(font . "Fira Code-12"))
  :config
  (delete-selection-mode)
  (load-theme 'modus-vivendi)
  (scroll-bar-mode -1)
  (column-number-mode 't)
  :bind
  ("C-<return>" . insert-line-below)
  ("C-S-<return>" . insert-line-above)
  :custom
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (display-line-numbers-type 'relative)
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  ;; Hide commands in M-x which do not work in the current mode
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Enable indentation+completion using the TAB key.
  (tab-always-indent 'complete)
  :hook
  (minibuffer-setup . cursor-intangible-mode)
  (prog-mode . display-line-numbers-mode)
  (org-mode . display-line-numbers-mode))

;; Checkers
(use-package flycheck
  :hook
  (prog-mode . flycheck-mode))

;; Completion
;;; main buffer
(use-package company
  :hook
  (after-init . global-company-mode))

(use-package company-box
  :hook
  (company-mode . company-box-mode))

;;; minibuffer
(use-package savehist
  :config
  (savehist-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :after (projectile)
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  :custom
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)

  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (consult-narrow-key "<") ;; "C-+"
  
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; consult--source-buffer :hidden t :default nil
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  
  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico
  :config
  (vertico-mode))

;; Tools

(use-package apheleia
  :custom
  (apheleia-formatters-respect-indent-level 'nil)
  :config
  (apheleia-global-mode +1))

(use-package multiple-cursors
  :bind
  ("C-c e m" . mc/edit-lines)
  ("C-c e n" . mc/mark-next-like-this)
  ("C-c e p" . mc/mark-previous-like-this)
  ("C-c e a" . mc/mark-all-like-this)
  ("C-c e r" . mc/mark-all-in-region)
  ("C-c e s" . mc/mark-sgml-tag-pair)
  ("C-c e d" . mc/mark-all-like-this-dwim))

(use-package direnv
  :config
  (direnv-mode))

(use-package which-key
  :config
  (which-key-mode))

(if (eq system-type 'windows-nt)
    (use-package eat)
  (use-package vterm))

;; vc
(use-package magit
  :bind
  ("C-c g" . magit-status))

(use-package diff-hl
  :after (magit)
  :config
  (global-diff-hl-mode)
  (diff-hl-dired-mode)
  (diff-hl-flydiff-mode)
  (diff-hl-show-hunk-mouse-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Projects/Workspaces
(use-package activities
  :config
  (activities-mode)
  (activities-tabs-mode)
  :bind
  (("C-c a n" . activities-new)
   ("C-c a d" . activities-define)
   ("C-c a a" . activities-resume)
   ("C-c a s" . activities-suspend)
   ("C-c a k" . activities-kill)
   ("C-c a RET" . activities-switch)
   ("C-c a b" . activities-switch-buffer)
   ("C-c a g" . activities-revert)
   ("C-c a l" . activities-list)))

(use-package projectile
  :demand t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package treemacs)

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

;; UI/UX
(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  :bind
  ("C-x o" . ace-window))

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                          '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                            ;; =:= =!=
                            ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                            ;; ;; ;;;
                            (";" (rx (+ ";")))
                            ;; && &&&
                            ("&" (rx (+ "&")))
                            ;; !! !!! !. !: !!. != !== !~
                            ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                            ;; ?? ??? ?:  ?=  ?.
                            ("?" (rx (or ":" "=" "\." (+ "?"))))
                            ;; %% %%%
                            ("%" (rx (+ "%")))
                            ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                            ;; |->>-||-<<-| |- |== ||=||
                            ;; |==>>==<<==<=>==//==/=!==:===>
                            ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                            "-" "=" ))))
                            ;; \\ \\\ \/
                            ("\\" (rx (or "/" (+ "\\"))))
                            ;; ++ +++ ++++ +>
                            ("+" (rx (or ">" (+ "+"))))
                            ;; :: ::: :::: :> :< := :// ::=
                            (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                            ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                            "="))))
                            ;; .. ... .... .= .- .? ..= ..<
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                            ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                            ;; *> */ *)  ** *** ****
                            ("*" (rx (or ">" "/" ")" (+ "*"))))
                            ;; www wwww
                            ("w" (rx (+ "w")))
                            ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                            ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                            ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                            ;; << <<< <<<<
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                            ;; >> >>> >>>>
                            (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                            ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                            ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
					 (+ "#"))))
                            ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ;; __ ___ ____ _|_ __|____|_
                            ("_" (rx (+ (or "_" "|"))))
                            ;; Fira code: 0xFF 0x12
                            ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                            ;; Fira code:
                            "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                            ;; The few not covered by the regexps.
                            "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package minions
  :config
  (minions-mode))

(use-package nerd-icons)

(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons)
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package nerd-icons-dired
  :after (nerd-icons)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :custom
  (doom-modeline-minor-modes t)
  (doom-modeline-total-line-number t)
  :hook (after-init . doom-modeline-mode))

(use-package dashboard
  :after
  (nerd-icons projectile org)
  :custom
  (dashboard-center-content t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-projects-backend 'projectile)
  (dashboard-display-icons-p t)
  (dashboard-items '((recents   . 5)
		     (agenda . 5)
                     (bookmarks  . 5)
                     (projects . 5)))
  (dashboard-startupify-list '(dashboard-insert-banner
			       dashboard-insert-newline
			       dashboard-insert-banner-title
			       dashboard-insert-newline
			       dashboard-insert-items
			       dashboard-insert-newline
			       dashboard-insert-init-info))
  :config
  (dashboard-setup-startup-hook))

(use-package wgrep)

;; lang
;;; org
(use-package org
  :hook
  (org-mode . auto-fill-mode)
  (org-mode . org-indent-mode)
  ;; (org-agenda-finalize . org-agenda-log-mode)
  :custom
  (org-agenda-files (list
		     (file-truename "~/org/agenda/")
		     (file-truename "~/org/agenda/projects/")))
  (org-agenda-show-future-repeats 'nil)
  (org-agenda-start-with-log-mode t)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-log-done 'time)
  :bind
  ("C-c o l" . org-store-link)
  ("C-c o a" . org-agenda)
  ("C-c o c" . org-capture))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/org/roam/"))
  :bind (("C-c o r l" . org-roam-buffer-toggle)
         ("C-c o r f" . org-roam-node-find)
         ("C-c o r g" . org-roam-graph)
         ("C-c o r i" . org-roam-node-insert)
         ("C-c o r c" . org-roam-capture))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package org-journal
  :custom
  (org-journal-prefix-key "C-c o j")
  (org-journal-file-type 'yearly)
  (org-journal-dir (file-truename "~/org/journal/"))
  (org-journal-file-format "%F.org"))

;;; treesitter
(use-package tree-sitter-langs)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; haskell
(use-package haskell-mode
  :hook (haskell-mode . tree-sitter-hl-mode))

;;; terraform
(use-package terraform-mode)

;;; lsp
(use-package lsp-haskell
  :after lsp-mode
  :custom
  (lsp-haskell-formatting-provider "fourmolu"))

(use-package lsp-pyright
  :after lsp-mode
  :custom (lsp-pyright-langserver-command "pyright")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "C-c c")
  :hook (
         (haskell-mode . lsp-deferred)
	 (haskell-literate-mode . lsp-deferred)
	 (typescript-ts-mode . lsp-deferred)
	 (javascript-mode . lsp-deferred)
	 (yaml-mode . lsp-deferred)
	 (terraform-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package consult-lsp
  :after (consult lsp-mode)
  :bind
  (:map lsp-mode-map
	("C-c c c d" . consult-lsp-diagnostics)
	("C-c c c f" . consult-lsp-file-symbols)
	("C-c c c s" . consult-lsp-symbols)))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode)
