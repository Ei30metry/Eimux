;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Artin Ghasivand
;;
;; Author: Artin Ghasivand <ghasivand.artin@gmail.com>
;; Maintainer: Artin Ghasivand <ghasivand.artin@gmail.com>
;; Created: August 05, 2022
;; Modified: August 05, 2022
;; Version: 0.0.1

;;; Code:
(setq gc-cons-threshold (* 50 1000 1000)
      read-process-output-max (* 1024 1024))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq emacs-started nil)

(setq locale-coding-system 'utf-8
      mac-command-modifier 'meta
      mac-option-modifier 'super)

(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(push (expand-file-name "~/.emacs.d/lisp") load-path)

(require 'env-stuff)
(require 'org-config)
(require 'buffer-config)
(require 'ui-config)
(require 'window-frame-config)
(require 'bookmark-register-config)
(require 'completion-config)
(require 'dired-config)
(require 'vc-config)
(require 'docs-config)
(require 'code-editing-config)
(require 'code-navigation-config)
(require 'lsp-config)
(require 'tex-bib-config)
(require 'terminal-shell-config)
(require 'document-viewing-config)
(use-package markdown-mode :straight t)
(require 'emacs-lisp-config)
(require 'haskell-config)
(require 'agda2-config)
(require 'idris2-config)
(require 'racket-config)
(require 'common-lisp-config)
(require 'ocaml-config)
(require 'lean-config)
(require 'nix-config)
(require 'rust-config)
(require 'web-langs-config)
(require 'python-config)
(provide 'swift-config)
(require 'serialization-stuff)
(require 'writing-utils)
(require 'mail-config)
(require 'telega-ement-config)
(require 'elfeed-config)
(require 'compilation-config)
(require 'misc)

(global-set-key (kbd "C-x C-. C-z r") #'(lambda () (interactive) (find-file "~/.zshrc")))
(global-set-key (kbd "C-x C-. C-z p") #'(lambda () (interactive) (find-file "~/.zprofile")))
(global-set-key (kbd "C-x C-. C-z e") #'(lambda () (interactive) (find-file "~/.zshenv")))
(global-set-key (kbd "C-x C-. C-e o") #'(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-x C-. C-e r") #'(lambda () (interactive) (load-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-x r j") 'consult-register-load)
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "C-x C-n"))
(global-unset-key (kbd "C-x C-j"))
(global-unset-key (kbd "C-x C-p"))
(global-unset-key (kbd "C-x C-v"))
(global-unset-key (kbd "C-x C-o"))
(global-unset-key (kbd "C-x C-w"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "M-j"))
(global-unset-key (kbd "C-x C-r"))
(global-unset-key (kbd "C-x C-d"))
(global-unset-key (kbd "C-M-u"))
(global-unset-key (kbd "C-M-m"))
(global-set-key (kbd "C-x C-' p") 'previous-buffer)
(global-set-key (kbd "C-x C-' n") 'next-buffer)
(global-set-key (kbd "C-x C-' l") 'persp-ibuffer)
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "M-RET") 'default-indent-new-line)
(global-set-key (kbd "C-x <C-m>") 'execute-extended-command)
(global-set-key (kbd "C-x p /") 'consult-ripgrep)
(global-set-key (kbd "C-x p b") 'consult-project-buffer)
(global-set-key (kbd "C-x p n") 'project-note-file)
(global-set-key (kbd "C-x p C") 'project-recompile)

(define-key input-decode-map [?\C-m] [C-m])

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs loaded in %s with %d garbage collections."
		     (format "%.3f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)
	    (setq emacs-started t)))
