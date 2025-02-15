;;; misc.el --- Description -*- lexical-binding: t; -*-

(setq set-mark-command-repeat-pop t)

(use-package hl-todo
  :straight t
  :demand t
  :init
  (global-hl-todo-mode))

(use-package lyrics-fetcher
  :straight t
  :after (emms))

(use-package read-player
  :straight (ready-player
	     :type git
	     :host github
	     :repo "xenodium/ready-player"
	     :files ("*.el" "data"))
  :commands (ready-player-mode))

(use-package biome :straight t)

(use-package aria2 :straight t)
(require 'aria2)

(use-package speed-type
    :straight t
    :hook
    (speed-type-mode . olivetti-mode)
    (speed-type-mode . (lambda () (interactive) (text-scale-set 4)))
    :bind
    (:map speed-type-mode-map
     ("C-i" . speed-type--replay))
    :config
    (setq speed-type-default-lang 'English))

(use-package bnf-mode :straight t)

(use-package verb :straight t)

(use-package epa
  :straight t)

(use-package pass
  :straight t)

(use-package prodigy
  :straight t)

(use-package w3m :straight t)

(use-package fireplace :straight t)

(add-hook 'comint-mode-hook #'(lambda () (display-line-numbers-mode 1)))

(defun comint-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

(use-package osx-plist
  :straight t)

(use-package org-tree-slide
  :straight t
  :config
  (setq org-image-actual-width nil))

(setq disabled-command-function nil)

(global-set-key (kbd "C-x C-. C-z r") #'(lambda () (interactive) (find-file "~/.zshrc")))
(global-set-key (kbd "C-x C-. C-z p") #'(lambda () (interactive) (find-file "~/.zprofile")))
(global-set-key (kbd "C-x C-. C-z e") #'(lambda () (interactive) (find-file "~/.zshenv")))
(global-set-key (kbd "C-x C-. C-e o") #'(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-x C-. C-e r") #'(lambda () (interactive) (load-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-x r j") 'consult-register-load)
(global-set-key (kbd "C-x C-' p") 'previous-buffer)
(global-set-key (kbd "C-x C-' n") 'next-buffer)
(global-set-key (kbd "C-x C-' l") 'persp-ibuffer)
(global-unset-key (kbd "M-j"))
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "M-RET") 'default-indent-new-line)
(global-set-key (kbd "C-x <C-m>") 'execute-extended-command)
(global-set-key (kbd "C-x p /") 'consult-ripgrep)
(global-set-key (kbd "C-x M-f") 'consult-fd)
(global-set-key (kbd "C-x p b") 'consult-project-buffer)
(global-set-key (kbd "C-x p n") 'project-note-file)
(global-set-key (kbd "C-x p C") 'project-recompile)

(global-unset-key (kbd "C-<mouse-5>"))
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

(provide 'misc)
