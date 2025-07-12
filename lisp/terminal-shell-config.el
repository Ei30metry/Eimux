;;; terminal-shell-config.el --- Description -*- lexical-binding: t; -*-

(use-package term
  :config
  (setq explicit-shell-file-name "zsh"))

(use-package vterm
  :demand t
  :bind
  ("C-x T" . vterm-other-window))

(global-unset-key (kbd "C-x t"))

(use-package eshell
  :bind
  ("C-x t" . eshell))

(use-package shell)

(use-package eat
  :straight
  (eat
   :type git
   :host codeberg
   :repo "akib/emacs-eat"
   :files ("*.el" ("term" "term/*.el") "*.texi"
           "*.ti" ("terminfo/e" "terminfo/e/*")
           ("terminfo/65" "terminfo/65/*")
           ("integration" "integration/*")
           (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package coterm :straight t)

(provide 'terminal-shell-config)
