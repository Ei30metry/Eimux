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
(require 'typst-config)
(require 'terminal-shell-config)
(require 'document-viewing-config)
(require 'emacs-lisp-config)
(require 'haskell-config)
(require 'agda2-config)
(require 'coq-config)
(require 'idris2-config)
(require 'racket-config)
(require 'common-lisp-config)
(require 'ocaml-config)
(require 'lean-config)
(require 'koka-config)
(require 'sql-config)
(require 'nix-config)
(require 'llvm-config)
(require 'c-c++-config)
(require 'rust-config)
(require 'zig-config)
(require 'web-langs-config)
(require 'python-config)
(require 'swift-config)
(require 'ott-config)
(use-package markdown-mode :straight t)
(require 'serialization-stuff)
(require 'writing-utils)
(require 'media-config)
(require 'mail-config)
(require 'telega-ement-config)
(require 'elfeed-config)
(require 'compilation-config)
(require 'misc)

(dolist (k '("C-x C-j" "C-x C-p"
             "C-x C-o" "C-x C-w"
             "C-z"     "C-x C-d"
             "C-M-m"))
  (global-unset-key (kbd k)))

(define-key input-decode-map [?\C-m] [C-m])

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs loaded in %s with %d garbage collections."
		     (format "%.3f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)
	    (setq emacs-started t)))
