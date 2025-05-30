;;; init.el --- Description -*- lexical-binding: t; -*-

(use-package sly
  :straight t
  :demand t
  :hook
  (common-lisp-mode . smartparens-strict-mode)
  (lisp-mode        . smartparens-strict-mode)
  (lisp-mode        . (lambda ()
                          (when (string= (buffer-name) "*sly-description*")
                            (display-line-numbers-mode -1))))
  (sly-mrepl-mode   . smartparens-strict-mode)
  :config
  (setq inferior-lisp-program (executable-find "sbcl")
        sly-symbol-completion-mode nil
        sly-complete-symbol-function #'sly-simple-completions)
  (setq-local )
  :bind
  (:map lisp-mode-map
        ("C-c C-d C-h" . nil)
        ("C-c C-d C-H" . sly-hyperspec-lookup)))

(use-package sly-macrostep
  :straight t)

(use-package sly-quicklisp
  :straight t
  :disabled)

(use-package sly-asdf :straight t
  :disabled)

(provide 'common-lisp-config)
