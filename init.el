;; ======== Better defaults ========
(setq-default
 inhibit-startup-message t
 inhibit-splash-screen t
 indicate-empty-lines t
 global-auto-revert-non-file-buffers t
 explicit-shell-file-name "/bin/bash"
 ;; Don't beep at me
 visible-bell t

 ;; Custom settings should automatically dump here
 custom-file "~/.emacs.d/custom.el"

 ;; Keep backups nicely
 backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Delete whitespace just when a file is saved.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Delete the region when typing, just like as we expect nowadays.
(delete-selection-mode t)

;; Automatic reloading of buffers
(global-auto-revert-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UI stuff
(progn
  ;; Do not show menu bar.
  (menu-bar-mode -1)

  ;; Do not show tool bar.
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

  ;; Do not show scroll bar.
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  ;; Display column number in mode line.
  (column-number-mode t)

  ;; Highlight line on point.
  (global-hl-line-mode t)

  ;; Show line numbers on left column
  (global-display-line-numbers-mode t)

  ;; Automatically revert when there are changes
  (global-auto-revert-mode t)

  ;; Show time
  (setq display-time-day-and-date t)
  (setq display-time-mail-string "")
  (display-time-mode t)

  ;; Nice font (Funky on debian, can't find system fonts)
  ;; (set-frame-font "Fira Code Retina-14" nil t)

  )

;; ====== Set up use=package and init.el management tools ======
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Load and activate emacs packages. Do this first so that the packages are loaded before
;; you start trying to modify them.  This also sets the load path.
(package-initialize)
(setq package-archive-priorities
      '(("gnu" . 3)
	("non-gnu" . 2)
	("melpa-stable" . 1)
	("melpa" . 0)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; This should come as a dependency of use-package but put this here
;; just to make it obvious we have it
(use-package bind-key)

;; ======================Configs===============================

(defun vsplit-other-window ()
  "Splits the window vertically and switches to that window."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))
(defun hsplit-other-window ()
  "Splits the window horizontally and switches to that window."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))

;; Themes
(use-package flatland-theme)
(use-package twilight-bright-theme)
(use-package zenburn-theme)
(use-package material-theme)
(load-theme 'material t)

(defun switch-theme (theme)
  "Disables any currently active themes and loads THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapc 'symbol-name
                                   (custom-available-themes))))))
  (let ((enabled-themes custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

;; (defun toggle-themes ()
;;   "Allow quick toggling between dark and light themes."
;;   (interactive)
;;   (if (member 'twilight-bright custom-enabled-themes)
;;       (progn
;; 	(load-theme 'flatland t)
;; 	(disable-theme 'twilight-bright))
;;     (load-theme 'twilight-bright t)
;;     (disable-theme 'flatland)))

(bind-key "M-l" 'move-to-window-line-top-bottom)
(bind-key "C-x 2" #'vsplit-other-window)
(bind-key "C-x 3" #'hsplit-other-window)
(bind-key "M-S-C-<left>" #'shrink-window-horizontally)
(bind-key "M-S-C-<right>" #'enlarge-window-horizontally)
(bind-key "M-S-C-<down>" #'shrink-window)
(bind-key "M-S-C-<up>" #'enlarge-window)
(bind-key "<end>" 'scroll-lock-mode)
;; (bind-key "M-[" 'goto-last-change)
;; (bind-key "M-]" 'goto-last-change-reverse)

;; ==================== Background "modes" ====================
(use-package delight)

(use-package auto-dim-other-buffers
  :config (auto-dim-other-buffers-mode -1))

(use-package company
  :delight
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1 company-idle-delay 0.0))

(use-package company-quickhelp
  :commands (company-quickhelp-mode)
  :ensure t
  :bind (:map company-active-map
              ("M-h" . company-quickhelp-manual-begin)))

(use-package smartparens
  :delight
  :config (require 'smartparens-config)
  :hook (prog-mode text-mode markdown-mode)
  ;; Useful commands
  ;; sp-splice-sexp
  ;; sp-rewrap-sexp
  :bind (("C-M-a" . sp-beginning-of-sexp)
	 ("C-M-e" . sp-end-of-sexp)

	 ("C-M-f" . sp-forward-sexp)
	 ("C-M-b" . sp-backward-sexp)

	 ("C-M-n" . sp-next-sexp)
	 ("C-M-p" . sp-previous-sexp)))

(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/backups/undo-tree"))))

(use-package helm
  :delight
  :config (helm-mode t)
  :bind (("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x C-f" . helm-find-files)
	 ("C-c h" . helm-command-prefix)
	 ("C-x b" . helm-mini)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-x r b" . helm-source-filtered-bookmarks)))

(use-package projectile
  :delight
  :config(projectile-mode t)
  (setq projectile-switch-project-action 'projectile-dired)
  :bind (("<insertchar>" . projectile-commander)
	 ("<insert>" . projectile-commander))
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'helm)
  (projectile-indexing-method 'hybrid))

(use-package projectile-ripgrep)

(use-package helm-projectile
  :pin melpa
  :config (helm-projectile-on))

(use-package helm-ag)

(use-package beacon
  :delight
  :config (beacon-mode t)
  :custom
  (beacon-blink-when-focused t)
  (beacon-color "#f6aa11"))

(use-package which-key
  :delight
  :config (which-key-mode t))

(use-package keycast
  :delight
  :config
  (keycast-header-line-mode t)
  (setq keycast-header-line-format "%2s%k%c%r"))

(use-package treemacs
  :init (with-eval-after-load 'winum
	  (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :hook (emacs-startup . treemacs)
  :config (setq treemacs-no-png-images t)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)))

(use-package treemacs-projectile
  :after (treemacs projectile))

;; (use-package activity-watch-mode
;;   :delight
;;   :config (global-activity-watch-mode -1))

;; ========================================

;; LSP Modes
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :config
  ;; The default setting is too low for lsp-mode's needs due to the
  ;; fact that client/server communication generates a lot of
  ;; memory/garbage.
  (setq gc-cons-threshold 100000000)
  ;; Increase the amount of data which Emacs reads from the process
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.5)
  (setq lsp-log-io nil)
  (setq lsp-ui-mode nil)
  :hook ((tuareg-mode . lsp-deferred)
	 ;; (prog-mode . lsp-deferred)
	 ;; (c-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  ;; :bind (("M-." . 'lsp-find-definition)
  ;; 	 ("M-r" . 'lsp-find-references))
  :custom ((lsp-modeline-code-actions-segments '(count icon name))
	   (lsp-headerline-breadcrumb-segments '(project file))
	   (lsp-headerline-breadcrumb-icons-enable nil)
	   (lsp-enable-on-type-formatting t)
	   (lsp-enable-imenu t))
  :commands (lsp lsp-deferred))

;; If you get connecting issues, run this command to wipe all the
;; compiled .elc files in .emacs.d/elpa
;; $ find . -type f -name '*.elc' -delete

(use-package yasnippet
  :hook (lsp-mode . yas-minor-mode))

(use-package flycheck)

(use-package lsp-treemacs
  :config (lsp-treemacs-sync-mode 1))

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :bind ("M-h" . 'lsp-ui-doc-glance)
;;   :custom
;;   (lsp-ui-doc-show-with-cursor nil)
;;   (lsp-ui-doc-enable t)
;;   (lsp-ui-doc-position 'at-point)
;;   (lsp-ui-sideline-enable t)
;;   (lsp-ui-sideline-show-code-actions t)
;;   (lsp-ui-peek-peek-height 5))

;; if you are helm user
(use-package helm-lsp
  :bind ([remap xref-find-apropos] . helm-lsp-workspace-symbol))

;; Tools
(use-package magit
  :after transient
  :bind ("C-c g" . magit))

(use-package ace-window
  :bind (("M-o" . ace-window)
	 ("C-M-O" . ace-swap-window)
	 ("M-d" . ace-delete-window)))

(use-package avy)
(use-package key-chord
  :config
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-define-global "jk" 'avy-goto-char-timer)
  (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
  (key-chord-define-global "uu" 'undo-tree-visualize)
  (key-chord-define-global "xx" 'execute-extended-command)
  ;; Keychord tips not very useful at the moment
  (defvar key-chord-tips '("Press <jj> quickly to jump to the beginning of a visible word."
                           "Press <jl> quickly to jump to a visible line."
                           "Press <jk> quickly to jump to a visible character."
                           "Press <JJ> quickly to switch to previous buffer."
                           "Press <uu> quickly to visualize the undo tree."
                           "Press <xx> quickly to execute extended command."
                           "Press <yy> quickly to browse the kill ring."))
  (key-chord-mode 1))

(use-package crux
  :pin melpa-stable
  :bind (("C-k" . crux-smart-kill-line)
	 ("C-c t" . crux-visit-term-buffer)
	 ("C-c r" . crux-rename-file-and-buffer)
	 ("C-c D" . crux-delete-file-and-buffer)
	 ("C-c I" . crux-find-user-init-file))
  :custom (crux-shell "/bin/fish"))

(use-package move-text
  :config (move-text-default-bindings))

(use-package iedit
  :bind (("C-c e" . iedit-mode)
	 (:map iedit-mode-keymap
	       ("M-h" . 'iedit-show/hide-context-lines))
	 (:map iedit-mode-occurrence-keymap
	       ("M-F" . 'iedit-restrict-function))))

(use-package expand-region
  :bind ("M-=" . er/expand-region))

(use-package paren
  ;; "Highlighting for parens"
  :custom
  (show-paren-delay 0.3)
  (blink-matching-paren t)
  (blink-matching-paren-on-screen t)
  (show-paren-style 'expression)
  (blink-matching-paren-dont-ignore-comments t)
  :config (show-paren-mode))

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(load "~/.emacs.d/ocaml.el")
