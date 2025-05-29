;; -*- lexical-binding: t -*-
(require 'package)

;; Config
(setenv "LSP_USE_PLISTS" "true")

;; Packages
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
  :config
  (setopt minibuffer-prompt-properties
	  '(read-only t cursor-intangible t face minibuffer-prompt))
  (setopt display-line-numbers-type 'relative)
  (setopt enable-recursive-minibuffers t)
  (setopt minibuffer-depth-indicate-mode t)
  ;; Hide commands in M-x which do not work in the current mode
  (setopt read-extended-command-predicate #'command-completion-default-include-p)
  ;; Enable indentation+completion using the TAB key.
  (setopt tab-always-indent 'complete)
  :hook
  (minibuffer-setup . cursor-intangible-mode)
  (prog-mode . display-line-numbers-mode)
  (org-mode . display-line-numbers-mode))

;; Checkers
(use-package flycheck
  :hook
  (prog-mode . flycheck-mode))

;; Completion
;;; minibuffer
(use-package savehist
  :config
  (savehist-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package orderless
  :config
  (setopt completion-styles '(orderless basic))
  (setopt completion-category-defaults nil)
  (setopt completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
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
         ("M-g f" . consult-flycheck)              ;; Alternative: consult-flymake
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

  :config
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setopt register-preview-delay 0.5)
  (setopt register-preview-function #'consult-register-format)

  ;; Use Consult to select xref locations with preview
  (setopt xref-show-xrefs-function #'consult-xref)
  (setopt xref-show-definitions-function #'consult-xref)

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setopt consult-narrow-key "<") ;; "C-+"
  
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

(use-package consult-flycheck
  :commands consult-flycheck)

(use-package consult-hoogle
  :commands consult-hoogle consult-hoogle-project)

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

  :config
  ;; Optionally replace the key help with a completing-read interface
  (setopt prefix-help-command #'embark-prefix-help-command)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after
  (embark)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico
  :config
  (vertico-mode))

;;; main buffer
(use-package company
  :hook
  (after-init . global-company-mode))

;; Tools
(use-package apheleia
  :config
  (setopt apheleia-formatters-respect-indent-level 'nil)
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
    (use-package eat
      :bind
      ("C-c t" . eat))
  (use-package vterm
    :bind
    ("C-c t" . vterm)))

;; vc
(use-package magit
  :bind
  ("C-c g" . magit-status))

(use-package diff-hl
  :after
  (magit)
  :config
  (global-diff-hl-mode)
  (diff-hl-dired-mode)
  (diff-hl-flydiff-mode)
  (diff-hl-show-hunk-mouse-mode)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

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

(use-package treemacs
  :commands treemacs)

(use-package treemacs-magit
  :after
  (treemacs))

;; UI/UX
(use-package ace-window
  :config
  (setopt aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setopt aw-scope 'frame)
  :bind
  ("C-x o" . ace-window))

(use-package ligature
  :config
  (setopt composition-break-at-point t)
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
  :after
  (treemacs)
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :config
  (setopt doom-modeline-minor-modes t)
  (setopt doom-modeline-total-line-number t)
  :hook (after-init . doom-modeline-mode))

(use-package dashboard
  :config
  (setopt dashboard-center-content t)
  (setopt dashboard-icon-type 'nerd-icons)
  (setopt dashboard-set-heading-icons t)
  (setopt dashboard-set-file-icons t)
  (setopt dashboard-projects-backend 'project-el)
  (setopt dashboard-display-icons-p t)
  (setopt dashboard-items '((recents   . 5)
			    (agenda . 5)
			    (bookmarks  . 5)
			    (projects . 5)))
  (setopt dashboard-startupify-list '(dashboard-insert-banner
				      dashboard-insert-newline
				      dashboard-insert-banner-title
				      dashboard-insert-newline
				      dashboard-insert-items
				      dashboard-insert-newline
				      dashboard-insert-init-info))
  (dashboard-setup-startup-hook))

(use-package wgrep)

;; lang
;;; org
(use-package org
  :hook
  (org-mode . auto-fill-mode)
  (org-mode . org-indent-mode)
  :config
  (setopt org-agenda-files (list
			    (file-truename "~/org/agenda/")
			    (file-truename "~/org/agenda/projects/")))
  (setopt org-agenda-show-future-repeats 'nil)
  (setopt org-agenda-start-with-log-mode 'clockcheck)
  (setopt org-enforce-todo-dependencies t)
  (setopt org-enforce-todo-checkbox-dependencies t)
  (setopt org-log-done 'time)
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/org/agenda/inbox.org" "Inbox")
           "* TODO %?\n  %i\n  %a")
	  ("j" "Journal" entry (file+datetree "~/org/agenda/journal.org")
           "* %?\nEntered on %T\n  %i\n  %a")))

  :bind
  ("C-c o l" . org-store-link)
  ("C-c o a" . org-agenda)
  ("C-c o c" . org-capture)
  ("C-c o t" . (lambda () (interactive) (org-capture nil "t")))
  ("C-c o j" . (lambda () (interactive) (org-capture nil "j"))))

;;; treesit
(use-package treesit-auto
  :config
  (setopt treesit-auto-install 'prompt)
  ;; grammars
   ;;; astro
  (let ((astro-recipe (make-treesit-auto-recipe
		       :lang 'astro
		       :ts-mode 'astro-ts-mode
		       :url "https://github.com/virchau13/tree-sitter-astro"
		       :revision "master"
		       :source-dir "src")))
    (add-to-list 'treesit-auto-recipe-list astro-recipe))
  ;; specify langs
  (setopt treesit-auto-langs
	  '(awk
	    bash
	    bibtex
	    c
	    cmake
	    commonlisp
	    css
	    dockerfile
	    go
	    gomod
	    java
	    javascript
	    json
	    lua
	    make
	    nix
	    org
	    python
	    rust
	    scala
	    sql
	    tsx
	    typescript
	    yaml))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (add-to-list 'treesit-auto-langs 'astro)
  ;; install and enable
  (treesit-auto-install-all)
  (global-treesit-auto-mode))

;;; haskell
(use-package haskell-mode)

;;; nix
(use-package nix-mode)

;;; terraform
(use-package terraform-mode)

;;; astro
(use-package astro-ts-mode
  :hook
  (astro-ts-mode . lsp-deferred))

;;; lsp
(use-package lsp-haskell
  :config
  (setopt lsp-haskell-formatting-provider "fourmolu")
  :hook ((haskell-mode . lsp-deferred)
	 (haskell-literature-mode . lsp-deferred)))

(use-package lsp-pyright
  :config
  (setopt lsp-pyright-langserver-command "pyright")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package lsp-mode
  :init
  ;; lsp booster setup
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

  (setq lsp-keymap-prefix "C-c c")
  :config
  (define-key lsp-mode-map (kbd "C-c c") lsp-command-map)
  :hook ((typescript-ts-mode . lsp-deferred)
	 (javascript-mode . lsp-deferred)
	 (yaml-mode . lsp-deferred)
	 (terraform-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package consult-lsp
  :after
  (lsp-mode)
  :bind
  (:map lsp-mode-map
	("C-c l d" . consult-lsp-diagnostics)
	("C-c l f" . consult-lsp-file-symbols)
	("C-c l s" . consult-lsp-symbols)))

(use-package dap-mode)

;; LLM
(use-package aidermacs
  :bind (("C-c v" . aidermacs-transient-menu))
  :config
  (setopt aidermacs-use-architect-mode t)
  (setopt aidermacs-default-model "openai/o4-mini"))
