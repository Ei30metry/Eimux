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
  :hook
  (ready-player-mode . ready-player-toggle-modeline))

(use-package biome :straight t)

(use-package aria2 :straight t)

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

(add-hook 'comint-mode-hook
          (lambda () (display-line-numbers-mode 1)))

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

(use-package leetcode
  :straight t
  :config
  (setq leetcode-directory "~/Programming/algorithms/leetcode"
        leetcode-prefer-language "racket"))

(use-package hackerrank
  :straight (hackerrank
             :type git
             :host github
             :repo "jun8git/emacs-hackerrank"
             :filed (".el")))

(setq disabled-command-function nil)

(use-package gcmh
  :straight t
  :config
  (gcmh-mode 1))

(use-package proced
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-descent t)
  (proced-filter 'user)
  :hook
  (proced-mode . (lambda () (proced-toggle-auto-update 1))))

(use-package proced-narrow :straight t :after proced)

(use-package emacs-everywhere :straight t)

(use-package devdocs :straight t)

(use-package exercism :straight t)

(use-package nyan-mode :straight t)

(use-package page-break-lines
  :straight t
  :config
  (global-page-break-lines-mode 1))

(defun delete-following-space (prefix)
  "Delete all of the space characters between point
and the first non-space character in front of it.
If the function is called with a prefix, it will call the
\\[delete-horizontal-space] command"
  (interactive "P")
  (if prefix
      (delete-horizontal-space)
    (let ((curr-point (point)))
      (skip-chars-forward "\s\t")
      (delete-region curr-point (point)))))

(defun load-init-file ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-x C-. C-z r") (lambda ()
                                        (interactive)
                                        (find-file "~/.zshrc")))
(global-set-key (kbd "C-x C-. C-z p") (lambda ()
                                        (interactive)
                                        (find-file "~/.zprofile")))
(global-set-key (kbd "C-x C-. C-z e") (lambda ()
                                        (interactive)
                                        (find-file "~/.zshenv")))
(global-set-key (kbd "C-x C-. C-e o") (lambda ()
                                        (interactive)
                                        (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-x C-. C-e r") 'load-init-file)
(global-set-key (kbd "C-x r j") 'consult-register-load)
(global-set-key (kbd "C-x C-' p") 'previous-buffer)
(global-set-key (kbd "C-x C-' n") 'next-buffer)
(global-set-key (kbd "C-x C-' l") 'persp-ibuffer)
(global-unset-key (kbd "M-j"))
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "M-RET") 'default-indent-new-line)
(global-set-key (kbd "C-x <C-m>") 'execute-extended-command)
(global-set-key (kbd "C-x M-f") 'consult-fd)
(global-set-key (kbd "M-\\") 'delete-following-space)
(global-set-key (kbd "M-/") 'dabbrev-completion)

(global-unset-key (kbd "C-<mouse-5>"))
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

(provide 'misc)
