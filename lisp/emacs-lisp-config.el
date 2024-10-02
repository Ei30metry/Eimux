;;; init.el --- Description -*- lexical-binding: t; -*-
(use-package macrostep :straight t)

(use-package dash :straight t)

(use-package llama :straight t)

(use-package esup
  :straight t)

(use-package ielm
  :bind
  (:map inferior-emacs-lisp-mode-map
        ("C-c C-z" . other-window)) ;; TODO write a proper switch-to-buffer
  :hook
  (inferior-emacs-lisp-mode . smartparens-strict-mode))

(use-package emacs-lisp-mode
  :config
  :bind
  (:map emacs-lisp-mode-map
   ("C-c C-k" . eval-buffer)
   ("C-c C-z" . ielm)) ;; TODO write a proper switch-to-ielm
  :hook
  (emacs-lisp-mode . smartparens-strict-mode))

(provide 'emacs-lisp-config)
