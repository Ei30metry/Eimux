;;; code-navigation-config.el --- Description -*- lexical-binding: t; -*-

(use-package imenu-anywhere :straight t)

(use-package imenu-list :straight t)

(use-package dumb-jump
    :straight t
    :demand t
    :config
    (setq dumb-jump-force-searcher 'ag))

(use-package xref
  :straight t
  :config
  (setq xref-prompt-for-identifier nil)
  :bind
  ("C-?" . xref-find-references-and-replace))

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(use-package avy
    :straight t
    :demand t
    :config (avy-setup-default)
    :bind ("C-;" . avy-goto-word-1)
          ("C-'" . avy-goto-char-in-line)
          ("<C-m> C-c" . avy-goto-char-2)
          ("<C-m> C-l" . avy-goto-line)
          ("<C-m> C-w" . avy-goto-word-1)
          ("<C-m> <C-m>" . avy-goto-word-1)
          (:map isearch-mode-map
                ("C-;" . avy-isearch)))

(use-package phi-search :straight t)

(use-package anzu
  :straight t
  :config
  (global-anzu-mode 1))

(use-package p-search :straight (:host github :repo "zkry/p-search"))

(use-package wgrep
  :straight t)

(provide 'code-navigation-config)
