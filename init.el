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
(setq gc-cons-threshold (* 50 1000 1000))

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

(use-package org
  :straight t
  :bind
  (:map org-mode-map
        ("M-g i" . consult-org-heading)))

(use-package exec-path-from-shell
 :straight t
 :config
 (setq exec-path-from-shell-arguments nil)
 (exec-path-from-shell-initialize))

(org-babel-load-file
     (expand-file-name
       "config.org"
       user-emacs-directory))

(setq read-process-output-max (* 1024 1024))

(setq emacs-started nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs loaded in %s with %d garbage collections."
		     (format "%.3f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)
	    (setq emacs-started t)))
