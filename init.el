;; ======== Better defaults ========
(setq-default
 inhibit-startup-message t
 inhibit-splash-screen t
 indicate-empty-lines t
 global-auto-revert-non-file-buffers t
 explicit-shell-file-name "/bin/fish"
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

  ;; Nice font (Funky on debian, can't find system fonts)
  (set-frame-font "Fira Code Retina-16" nil t)

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

;; ============================================================

;; Configs
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

(bind-key "C-x 2" #'vsplit-other-window)
(bind-key "C-x 3" #'hsplit-other-window)
(bind-key "M-S-C-<left>" #'shrink-window-horizontally)
(bind-key "M-S-C-<right>" #'enlarge-window-horizontally)
(bind-key "M-S-C-<down>" #'shrink-window)
(bind-key "M-S-C-<up>" #'enlarge-window)
(bind-key "<end>" 'scroll-lock-mode)

;; ==================== Background "modes" ====================
(use-package delight)

(use-package company
  :delight
  :hook (after-init . global-company-mode))

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
  (projectile-indexing-method 'hybrid))

(use-package helm-projectile
  :pin melpa
  :config (helm-projectile-on))

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
  :config (keycast-header-line-mode t))

(use-package activity-watch-mode
  :delight
  :config (global-activity-watch-mode -1))

;; ========================================

;; LSP Modes
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-peek-peek-height 5))
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

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

;; Themes
(use-package flatland-theme
  :config (load-theme 'flatland t))
(use-package twilight-bright-theme)
(use-package zenburn-theme)

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

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(load "~/.emacs.d/ocaml.el")
