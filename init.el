;; ======== Better defaults ========
(setq-default
 inhibit-startup-message t
 inhibit-splash-screen t
 indicate-empty-lines t
 global-auto-revert-non-file-buffers t
 shell-file-name "/bin/bash"
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

  ;; Show line numbers on left column, only in prog mode
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

;; ====== Set up straight.el and use-package ======
;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Disable package.el to avoid conflicts
(setq package-enable-at-startup nil)

;; Install and configure use-package with straight.el
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
;; This makes :straight t the default for all use-package forms
(setq straight-use-package-by-default t)
;; Note: use-package-always-ensure is for package.el, not straight.el

;; This should come as a dependency of use-package but put this here
;; just to make it obvious we have it
(use-package bind-key)

;; ====================== Adhoc Configs ===============================

(defun git-log-region-authors ()
  "Show git authors and dates who modified the selected region."
  (interactive)
  (let* ((file (buffer-file-name))
         (start-line (line-number-at-pos (region-beginning)))
         (end-line (line-number-at-pos (region-end)))
         (git-root (vc-git-root file))
         (relative-file (file-relative-name file git-root))
         (default-directory git-root)  ; Change to git root before running command
         (cmd (format "git log -L %d,%d:%s --pretty=format:'%%an %%ad' --date=short"
                      start-line end-line relative-file)))
    (if (not file)
        (message "Buffer is not visiting a file")
      (if (not git-root)
          (message "File is not in a git repository")
        (let ((output (shell-command-to-string cmd)))
          (with-output-to-temp-buffer "*Git Log Authors*"
            (princ output)))))))

;; Themes
(use-package flatland-theme)
(use-package twilight-bright-theme)
(use-package zenburn-theme)
(use-package material-theme)
(load-theme 'flatland t)

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

(bind-key "<end>" 'scroll-lock-mode)

;; ==================== Background "modes" ====================

;; Enable Vertico.
(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Example configuration for Consult
(use-package consult
  ;; Install consult version 2.8 from git repository
  :straight (consult :type git :host github :repo "minad/consult" :tag "2.8")
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
         ("M-g r" . consult-grep-match)
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
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ("M-r" . consult-ripgrep)
         ("M-l" . consult-line)
         ;; ("m-s L" . consult-line-multi)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
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
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

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
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Install marginalia version 2.3 from git repository
  :straight (marginalia :type git :host github :repo "minad/marginalia" :tag "2.3")
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  )

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/backups/undo-tree"))))

(use-package projectile
  :delight
  :config(projectile-mode t)
  (setq projectile-switch-project-action 'projectile-dired)
  :bind (("<insertchar>" . projectile-commander)
	 ("<insert>" . projectile-commander))
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'auto)
  (projectile-indexing-method 'alien)
  (projectile-enable-caching 'persistent)
  (projectile-project-root-files-functions
   '(projectile-root-marked
     projectile-root-local
     projectile-root-bottom-up
     )))

(use-package keycast
  :config
  (keycast-header-line-mode t)
  (setq keycast-header-line-format "%2s%k%c%r"))

;; Tools
(use-package magit
  :after transient
  :bind ("C-c g" . magit))

(use-package ace-window
  :bind (("M-o" . ace-window)
	 ("C-M-O" . ace-swap-window)
	 ("M-d" . ace-delete-window)))

(use-package avy
  :bind ("M-j" . 'avy-goto-char-timer))

(use-package key-chord
  :config
  (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
  (key-chord-define-global "uu" 'undo-tree-visualize)
  ;; Keychord tips not very useful at the moment
  (defvar key-chord-tips '("Press <JJ> quickly to switch to previous buffer."
                           "Press <uu> quickly to visualize the undo tree."))
  (key-chord-mode 1))

;; (use-package crux
;;   :pin melpa-stable
;;   :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
;; 	 ("C-k" . crux-smart-kill-line)
;; 	 ;; ("C-c t" . crux-visit-term-buffer)
;; 	 ("C-c r" . crux-rename-file-and-buffer)
;; 	 ("C-c D" . crux-delete-file-and-buffer)
;; 	 ("C-c I" . crux-find-user-init-file))
;;   :custom (crux-shell "/bin/bash"))

;; (use-package move-text
;;   :config (move-text-default-bindings))

;; (use-package iedit
;;   :bind (("C-c e" . iedit-mode)
;; 	 (:map iedit-mode-keymap
;; 	       ("M-h" . 'iedit-show/hide-context-lines))
;; 	 (:map iedit-mode-occurrence-keymap
;; 	       ("M-F" . 'iedit-restrict-function))))

;; (use-package expand-region
;;   :bind ("M-=" . er/expand-region))

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(load "~/.emacs.d/ocaml.el")
