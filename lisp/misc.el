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

(use-package epa
  :straight t)

(use-package pass
  :straight t)

(use-package prodigy
  :straight t)

(use-package w3m :straight t)

(add-hook 'comint-mode-hook #'(lambda () (display-line-numbers-mode 1)))

(defun comint-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

(use-package osx-plist
  :straight t)

(setq disabled-command-function nil)

(provide 'misc)
