(use-package tuareg)

(use-package merlin
  :custom
  (merlin-completion-with-doc t)
  (merlin-error-check-then-move nil)
  (merlin-command 'opam)
  (merlin-error-after-save t)
  (merlin-locate-preference 'mli)
  :bind (:map merlin-mode-map
              ("M-." . merlin-locate)
              ("M-," . merlin-pop-stack)
              ("M-?" . merlin-occurrences)
              ("C-c m j" . merlin-jump)
              ("C-c m i" . merlin-locate-ident)
              ("C-c m e" . merlin-iedit-occurrences)
              ("C-c m d" . merlin-document))
  :hook
  ;; Start merlin on ml files
  (tuareg-mode . merlin-mode))

(use-package merlin-eldoc
  :custom
  (eldoc-echo-area-use-multiline-p t)
  (merlin-eldoc-max-lines 8)
  (merlin-eldoc-max-lines-function-arguments 1)
  (merlin-eldoc-type-verbosity 'min)
  (merlin-eldoc-function-arguments nil)
  (merlin-eldoc-doc nil)
  :bind (:map merlin-mode-map
              ("C-c m p" . merlin-eldoc-jump-to-prev-occurrence)
              ("C-c m n" . merlin-eldoc-jump-to-next-occurrence))
  :hook ((tuareg-mode reason-mode) . merlin-eldoc-setup))

(use-package merlin-iedit)
(use-package dune-format)
(use-package ocamlformat
  :commands (ocamlformat ocamlformat-before-save)
  :bind (:map tuareg-mode-map
              ("M-<iso-lefttab>" . ocamlformat)))
