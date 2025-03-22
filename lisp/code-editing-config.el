;;; code-editing-config.el --- Description -*- lexical-binding: t; -*-

(delete-selection-mode t)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq case-fold-search nil)

(use-package iedit
  :straight t
  :bind
  ("C-M-;" . iedit-mode))

(use-package easy-kill
  :straight t
  :bind
  ("M-w" . easy-kill)
  ("C-M-SPC" . easy-mark))

(use-package yasnippet :straight t)

(global-set-key (kbd "C-x M-a M-a") 'align)
(global-set-key (kbd "C-x M-a M-r") 'align-regexp)
(global-set-key (kbd "C-x M-a M-c") 'align-current)
(global-set-key (kbd "C-x M-a M-e") 'align-entire)

(use-package format-all :straight t)

(use-package zzz-to-char
  :straight t
  :bind ("M-z" . zzz-to-char-up-to-char)
  :demand t)

(global-set-key (kbd "<C-m> C-o") 'occur)

(use-package multiple-cursors
  :straight t
  :bind
  ("C->" . mc/edit-ends-of-lines)
  ("C-<" . mc/edit-beginnings-of-lines))

(use-package vundo
  :straight t
  :demand t
  :bind
  ("C-x u" . vundo))

(use-package undo-fu
  :demand t
  :straight t)

(use-package smartparens
  :straight t
  :demand t
  :config
  (require 'smartparens-haskell)
  (require 'smartparens-racket)
  (require 'smartparens-config)
  :bind
  ("C-M-d" . beginning-of-defun)
  ("C-M-c" . end-of-defun)
  ("C-M-a" . sp-beginning-of-sexp)
  ("C-M-e" . sp-end-of-sexp)
  ("C-M-'" . sp-raise-sexp)
  ("M-["   . sp-backward-down-sexp)
  ("C-M-[" . sp-backward-up-sexp)
  ("M-]"   . sp-down-sexp)
  ("C-M-]" . sp-up-sexp)
  ("C-M-f" . sp-forward-sexp)
  ("C-M-b" . sp-backward-sexp)
  ("C-M-n" . sp-next-sexp)
  ("C-M-p" . sp-previous-sexp)
  ("C-S-b" . sp-backward-symbol)
  ("C-S-f" . sp-forward-symbol)
  ("C-S-d" . sp-kill-symbol)
  ("C-S-<backspace>" . sp-backward-kill-symbol)
  ("M-S-<backspace>" . sp-backward-kill-sexp)
  ("C-M-<backspace>" . sp-delete-symbol)
  ("C-M-k" . sp-kill-sexp)
  ("C-S-k" . sp-kill-symbol)
  ("C-M-u" . sp-forward-slurp-sexp)
  ("C-S-u" . sp-backward-slurp-sexp)
  ("C-M-y" . sp-forward-barf-sexp)
  ("C-S-y" . sp-backward-barf-sexp)
  ("C-M-w" . sp-copy-sexp)
  ("C-c (" . sp-wrap-round)
  ("C-c [" . sp-wrap-square)
  ("C-c {" . sp-wrap-curly)
  ("C-c u" . sp-unwrap-sexp)
  ("C-c r" . sp-rewrap-sexp)
  ("C-M-j" . sp-join-sexp)
  ("C-M-g" . sp-split-sexp)
  ("C-c U" . sp-backward-unwrap-sexp)
  :hook
  (prog-mode . smartparens-mode)
  :init
  (show-smartparens-global-mode))

(provide 'code-editing-config)
