;;; terminal-shell-config.el --- Description -*- lexical-binding: t; -*-

(use-package term
  :config
  (setq explicit-shell-file-name "zsh"))
;; :hook
;; (term-mode . compilation-shell-minor-mode)


(use-package vterm
  :demand t
  ;; :hook
  ;; (vterm-mode . compilation-shell-minor-mode)
  :bind
  ("C-x T" . vterm-other-window))

(global-unset-key (kbd "C-x t"))

(use-package eshell
  ;; :hook
  ;; (eshell-mode . compilation-shell-minor-mode)
  :bind
  ("C-x t" . eshell))

(use-package shell)
  ;; :hook (shell-mode . compilation-shell-minor-mode)

(provide 'terminal-shell-config)
