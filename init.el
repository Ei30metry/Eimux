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

(setq straight-profiles '(('default . "default.el")
                          ('tried-and-true . "tried-and-true.el")))

(setq straight-current-profile 'default)

(straight-use-package 'org)

(use-package exec-path-from-shell
 :straight t
 :config
 (setq exec-path-from-shell-arguments nil)
 (exec-path-from-shell-initialize))

(push (expand-file-name "~/.emacs.d/lisp") load-path)

(define-key input-decode-map [?\C-m] [C-m])

(setq read-process-output-max (* 1024 1024))

(setq emacs-started nil)

(setq inhibit-startup-screen t
      inhibit-startup-message t)

(winner-mode 1)
(setq display-line-numbers-type 'relative
      scroll-conservatively 101)
   (if (display-graphic-p)
       (menu-bar-mode 1)
       (menu-bar-mode -1))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(pixel-scroll-precision-mode 1)
(global-visual-line-mode 1)

(use-package ns-auto-titlebar :straight t :config (ns-auto-titlebar-mode 1))

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(use-package mood-line
  :straight t)

(column-number-mode 1)

(add-hook 'prog-mode-hook #'(lambda () (display-line-numbers-mode 1)))
(global-set-key (kbd "<C-m> C-n") 'next-error)
(global-set-key (kbd "<C-m> C-p") 'previous-error)

(add-hook 'comint-mode-hook #'(lambda () (display-line-numbers-mode 1)))

(setq disabled-command-function nil)

(delete-selection-mode t)

(require 'hl-line)

(global-hl-line-mode t)

(setq custom-file (make-temp-name "/tmp/"))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq case-fold-search nil)

(defun camlcase-to-snakecase () (message "TODO"))
(defun snakecase-to-camlcase () (message "TODO"))

(global-set-key (kbd "C-x C-. C-z r") #'(lambda () (interactive) (find-file "~/.zshrc")))
(global-set-key (kbd "C-x C-. C-z p") #'(lambda () (interactive) (find-file "~/.zprofile")))
(global-set-key (kbd "C-x C-. C-z e") #'(lambda () (interactive) (find-file "~/.zshenv")))

(setq-default message-log-max :error)
(setq initial-scratch-message nil)

(setq initial-major-mode 'emacs-lisp-mode)
(setq switch-to-buffer-obey-display-actions t)

(setq display-buffer-alist
      '(
        ((or . ((derived-mode . helpful-mode)
                (derived-mode . idris2-info-mode)
               "\\*\\(Help\\|haskell-compilation\\|compilation\\|sly-description\\|sly-macroexpansion\\|toc\\)\\*"))
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
          (window-height . 20)
          (dedicated . t)
          (body-function . select-window))

        ("\\*\\(hoogle\\|eldoc for*\\|osx-dictionary\\)\\*"
         (display-buffer-at-bottom)
          (window-height . 15)
          (dedicated . t)
          (body-function . select-window))

        ("\\*Occur\\*"
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
         (window-height . 20)
         (dedicated . t)
         (body-function . select-window))
        ;; NOTE I think we I can replace this by saying comint-mode or ...
        ((or . ((derived-mode . haskell-interactive-mode)
                (derived-mode . sly-mrepl-mode)
                (derived-mode . inferior-emacs-lisp-mode)
               "\\*\\(vterm\\|shell\\|eshell\\|terminal\\|ielm\\|Nix-REPL\\|haskell\\|Racket
REPL </>\\|Racket Describe </>\\|Racket Logger </>\\|Tex Help\\|idris2-repl\\|terminal\\)\\*"))
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
          (window-height . 22)
          (body-function . select-window))

        ("\\*Org Select\\*"
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
         (window-height . 20)
         (body-function . select-window))
        ("\\*Org Src*"
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
         (window-height . 45)
         (body-function . select-window))
        ("Capture-*"
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
         (window-height . 20)
         (nil . t))

        ((derived-mode . pdf-outline-buffer-mode)
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
         (window-height . 20)
         (dedicated . t)
         (body-function-select-window))

        ("\\*Async Shell Command\\*"
         (display-buffer-no-window)
         (allow-no-window . t))

        ("\\*Warnings\\*"
         (display-buffer-no-window)
         (allow-no-window . t))
       ))

(add-hook 'minibuffer-exit-hook
      #'(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
            (kill-buffer buffer)))))

(use-package mode-local
  :straight t)

(setq-default show-trailing-whitespace nil)

(setq-mode-local show-trailing-whitespace t)

(setq-default warning-minimum-level :error)
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil)

(save-place-mode 1)

(global-set-key (kbd "C-x 4 x g") 'revert-other-buffer-quick)

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer)

(defun revert-other-buffer-quick ()
  (interactive)
  (message "TODO"))

(setq register-preview-delay nil)
(global-set-key (kbd "C-x r j") 'consult-register-load)

(use-package bm
  :straight t)

(use-package bookmark-view
  :straight t)

(setq set-mark-command-repeat-pop t)

(use-package perspective
  :straight t
  :custom
  (persp-mode-prefix-key (kbd "C-x C-,"))
  :bind
  ("C-x k" . (lambda () (interactive) (persp-kill-buffer* nil)))
  ("C-x K" . persp-kill-buffer*)
  ("C-." . persp-switch-to-buffer*)
  ("C-x b" . switch-to-buffer)
  :init
  (setq persp-initial-frame-name "misc")
  (persp-mode))

(global-set-key (kbd "C-x w m") 'maximize-window)
(global-set-key (kbd "C-x w u") 'winner-undo)
(global-set-key (kbd "C-x w r") 'winner-redo)
(use-package transpose-frame :straight t)

(use-package popper
  :straight t
  :bind
  ("C-x C-' f" . popper-cycle)
  ("C-x C-' b" . popper-cycle-backwards)
  ("C-,"  . popper-toggle)
  ("C-x C-' t" . popper-toggle-type)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Help\\*"
          "\\*hoogle\\*"
          "\\*haskell\\*"
          "\\*Tex Help\\*"
          "\\*toc\\*"
          "\\*Occur\\*"
          "\\*eldoc for\\*$"
          "Output\\*$"
          "\\*Backtrace\\*"
          "\\*Async Shell Command\\*"
          "\\*sly-macroexpansion\\*"
          "\\*sly-description\\*"
          help-mode
          compilation-mode
          haskell-interactive-mode
          comint-mode
          vterm-mode
          pdf-outline-buffer-mode
          helpful-mode
          osx-dictionary-mode
          racket-repl-mode
          nix-repl-mode
          idris2-repl-mode
          idris2-info-mode
          idris2-compiler-notes-mode
          sly-mrepl-mode
          inferior-emacs-lisp-mode
          term-mode
          eshell-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package doom-themes
   :straight t
   :config
   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
         doom-themes-enable-italic t)
   (doom-themes-visual-bell-config)
   (doom-themes-org-config)
   :init
   (load-theme 'doom-sourcerer t))

(set-face-attribute 'default nil
                    :font "JetBrains Mono 13"
                    :weight 'medium)

(set-face-attribute 'variable-pitch nil
                    :font "JetBrains Mono 13"
                    :weight 'medium)

(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono 13"
                    :weight 'medium)

(add-to-list 'default-frame-alist '(font . "JetBrains Mono 13"))

(use-package ligature :straight t)

;; (set-fontset-font t nil "SF Pro Display" nil 'append)

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

(global-set-key (kbd "C-x C-. C-e o") #'(lambda () (interactive) (find-file "~/.emacs.d/config.el")))
(global-set-key (kbd "C-x C-. C-e r") #'(lambda () (interactive) (load-file "~/.emacs.d/init.el")))

(require 'ibuffer)
(global-set-key (kbd "C-x C-' p") 'previous-buffer)
(global-set-key (kbd "C-x C-' n") 'next-buffer)
(global-set-key (kbd "C-x C-' l") 'persp-ibuffer)
(global-set-key (kbd "C-x C-' s") 'scratch-buffer)
(global-set-key (kbd "C-S-z") 'zap-up-to-space)
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "M-RET") 'default-indent-new-line)
(global-set-key (kbd "C-x C-n") 'next-error)
(global-set-key (kbd "C-x C-p") 'previous-error)

(global-set-key (kbd "C-x <C-m>") 'execute-extended-command)

(setq mac-command-modifier 'meta
      mac-option-modifier 'super)

(use-package osx-plist
  :straight t)

(setq confirm-kill-emacs 'y-or-n-p)

(defun ask-before-closing ()
  "Close only if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Really close frame? "))
      (save-buffers-kill-emacs)
    (message "Canceled frame close")))

(when (daemonp)
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

(use-package org
  :bind
  ("C-x A" . org-agenda)
  ("C-," . popper-toggle)
  ("C-S-c" . org-capture)
  (:map org-mode-map ("C-S-c" . org-capture) ("C-," . nil) ("C-'" . nil))
  :config
  (setq org-startup-indented t
        org-directory "~/Agenda"
        org-log-into-drawer t
        org-treat-insert-todo-heading-as-state-change t
        org-hide-emphasis-markers t
        org-return-follows-link t
        org-src-tab-acts-natively nil
        org-agenda-files '("~/Agenda/tasks.org"  "~/Agenda/projects/specification.org"))
  :hook
  (org-agenda-mode . (lambda () (visual-line-mode -1) (toggle-truncate-lines 1))))

(require 'org-tempo)
(setq org-structure-template-alist
      '(("el" . "src emacs-lisp")
        ("py" . "src python")
        ("sq" . "src sql")
        ("hs" . "src haskell")
        ("t" . "src tex")
        ("rs" . "src rust")
        ("c"  . "src c")
        ("tx" . "src txt")
        ("o" . "src ott")))

(use-package org-books
 :straight t
 :after org
 :config
 (setq org-books-file "~/Agenda/books.org"))

(setq org-capture-templates
     '(("t" "Task")
       ("tt" "Planned" entry (file+headline "tasks.org" "Planned") "* TODO %?\nSCHEDULED: %^t\nDEADLINE: %^t")
       ("tT" "Today" entry (file+headline "tasks.org" "Planned") "* TODO %?\nSCHEDULED: %t\nDEADLINE: %t")
       ("tl" "Process later" entry (file+headline "tasks.org" "Inbox") "* TODO %?")
       ("tp" "Project")
       ("tps" "Specification" entry (file+headline "projects/specification.org" "Tasks") "* TODO %?")
       ("tpg" "GHC" entry (file+headline "projects/ghc.org" "Tasks") "* TODO %?")
       ("tc" "Config")
       ("tce" "Emacs" entry (file+headline "config/emacs-config.org" "Tasks") "* TODO %?\n")
       ("tcn" "Nix" entry (file+headline "config/nix.org" "Tasks") "* TODO %?\n  %i")
       ("a" "Ask" entry (file+headline "projects/specification.org" "QUESTIONS") "* QUESTION %?\n")))

(use-package org-roam
   :straight t
   :after org
   :bind
   ("C-x C-r C-r"     . org-roam-capture)
   ("C-x C-r C-t"     . org-roam-dailies-capture-today)
   ("C-x C-r C-j t"   . org-roam-dailies-goto-today)
   ("C-x C-r w"       . org-roam-refile)
   ("C-x C-r C-j y"   . org-roam-dailies-goto-yesterday)
   ("C-x C-r C-j C-d" . org-roam-dailies-find-directory)
   ("C-x C-r C-j n"   . org-roam-dailies-goto-next-note)
   ("C-x C-r C-j p"   . org-roam-dailies-goto-previous-note)
   ("C-x C-r C-j d"   . org-roam-dailies-goto-date)
   ("C-x C-r b"       . org-roam-buffer-display-dedicated)
   ("C-x C-r C-i r"   . org-roam-ref-add)
   ("C-x C-r C-i t"   . org-roam-tag-add)
   ("C-x C-r C-i a"   . org-roam-alias-add)
   ("C-x C-r C-i n"   . org-roam-node-insert)
   ("C-x C-r C-f"     . org-roam-node-find)
   (:map org-roam-mode-map ("M-." . org-roam-ref-find))
   :config
   (setq org-roam-directory "~/Roam"
         org-roam-db-autosync-mode t))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :bind
    ("C-x C-r C-u" . org-roam-ui-open)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package org-roam-bibtex
  :straight t)

(use-package toc-org
 :straight t
 :after org
 :hook
 (org-mode . toc-org-mode))

(use-package consult
   :straight t
   :demand t
   :bind
   ("<C-m> C-i" . consult-imenu)
   ("<C-m> C-s" . consult-line)
   ("M-y" . yank-pop)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("<C-m> C-d" . consult-mark)
   ("M-g M-m" . consult-mark)
   (:map org-mode-map
   ("<C-m> C-i" . consult-org-heading)))

(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(use-package consult-eglot
  :straight t
  :after eglot)

(use-package embark-consult :straight t)



(use-package consult-omni
    :straight (consult-omni
               :type git
               :host github
               :repo "armindarvish/consult-omni"
               :files (:defaults "sources/*.el"))
    :config
    (straight-use-package 'request)
    (setq consult-omni-show-preview t
          consult-omni-preview-key "C-i"
          consult-omni-default-count 5
          consult-omni-default-input-throttle 1.7
          consult-embark-default-term #'vterm
          consult-omni-default-browse-function 'browse-url
          consult-omni-default-interactive-command #'consult-omni-multi
          consult-omni-http-retrieve-backend 'request
          consult-omni-open-with-prompt "λ. ")
    (require 'consult-omni-sources)
    (require 'consult-omni-embark)
    (consult-omni-sources-load-modules)
    (setq consult-omni-multi-sources '("calc"
                                       "File"
                                       "Apps"
                                       "Google"
                                       "GitHub"
                                       "Org Agenda")
          consult-omni-web-sources '("Wikipedia"
                                     "Github")))

(defun consult-omni-web (&optional initial prompt sources no-callback &rest args)
  "Interactive web search”

This is similar to `consult-omni-multi', but runs the search on
web sources defined in `consult-omni-web-sources'.  See
`consult-omni-multi' for more details.
"
  (interactive "P")
  (let ((prompt (or prompt (concat "[" (propertize "consult-omni-web" 'face 'consult-omni-prompt-face) "]" " Search:  ")))
        (sources (or sources consult-omni-web-sources)))
    (consult-omni-multi initial prompt sources no-callback args)))

(use-package consult-notmuch
  :straight t
  :after notmuch)

(use-package embark
    :straight t
    :demand t
    :bind
    (:map minibuffer-mode-map
    ("C-." . embark-act))
    :config
    (setq prefix-help-command #'embark-prefix-help-command))

(use-package imenu-anywhere :straight t)

(use-package imenu-list :straight t)

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package helpful
  :straight t
  :demand t
  :bind
  ("C-h k" . helpful-key)
  ("C-h v" . helpful-variable)
  ("C-h f" . helpful-callable)
  ("C-h x" . helpful-command)
  ("C-h ." . helpful-at-point)
  ("C-h q" . helpful-kill-buffers))

(use-package iedit
  :straight t
  :bind
  ("C-M-;" . iedit-mode))

(use-package easy-kill
  :straight t
  :bind
  ("M-w" . easy-kill)
  ("C-M-SPC" . easy-mark))

(use-package discover-my-major
  :straight t
  :bind
  ("C-h <C-m> . discover-my-major")
  ("C-h M-m" . discover-my-mode))

(global-set-key (kbd "C-x p /") 'consult-ripgrep)
(global-set-key (kbd "C-x p b") 'consult-project-buffer)
(global-set-key (kbd "C-x p n") 'project-note-file)
(global-set-key (kbd "C-x p C") 'project-recompile)

(use-package direnv :straight t)

(use-package direnv :straight t)

(use-package yasnippet :straight t)

(global-set-key (kbd "C-x M-a M-a") 'align)
(global-set-key (kbd "C-x M-a M-r") 'align-regexp)
(global-set-key (kbd "C-x M-a M-c") 'align-current)
(global-set-key (kbd "C-x M-a M-e") 'align-entire)

(use-package format-all :straight t)

(defun comint-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

(use-package compile
  :bind
  (:map compilation-mode-map
   ("c" . project-compile))
  :hook (compilation-filter . comint-ansi-color-process-output)
  :config
  (setq compilation-always-kill t))

(use-package imake :straight t)



(use-package diredfl
  :straight t
  :init
  (diredfl-global-mode))

(use-package dired
  :bind
  (:map dired-mode-map
   ("C-j" . dired-jump))
  :config
  (setq dired-kill-when-opening-new-dired-buffer t))

(use-package ace-window
        :straight t
        :demand t
        :config
        (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
              aw-dispatch-always t)
        :bind
        ("C-x o" . other-window)
        ("M-o" . ace-window))

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

(use-package zzz-to-char
  :straight t
  :bind ("M-z" . zzz-to-char-up-to-char)
  :demand t)

(global-set-key (kbd "<C-m> C-o") 'occur)

(use-package multiple-cursors :straight t)

(use-package vundo
  :straight t
  :demand t
  :bind
  ("C-x u" . vundo))

(use-package envrc
  :straight t
  :hook
  (after-init . envrc-global-mode))

(use-package undo-fu
  :demand t
  :straight t)

(use-package magit
  :straight t
  :demand t
  :commands magit-status)

(use-package forge
  :straight t)

(setq gc-cons-threshold 100000000)
(use-package eglot
  :ensure nil
  :commands eglot
  :bind
  ("C-c C-e C-e" . eglot)
  (:map eglot-mode-map
  ("C-c C-s" . consult-eglot-symbols)
  ("C-c C-." . eldoc)
  ("C-c C-e C-f" . consult-flymake)
  ("<C-m> C-n" . flymake-goto-next-error)
  ("<C-m> C-p" . flymake-goto-prev-error)
  ("C-c C-a C-c" . eglot-code-actions)
  ("C-c C-e C-t" . eglot-find-typeDefinition)
  ("C-c C-a C-i" . eglot-code-action-inline)
  ("C-c C-a C-e" . eglot-code-action-extract)
  ("C-c C-a C-o" . eglot-code-action-organize-imports)
  ("C-c C-a C-r" . eglot-code-action-rewrite)
  ("C-c C-a C-a" . eglot-code-action-quickfix)
  ("C-c C-e C-r" . eglot-rename)
  ("C-c C-e C-s C-r" . eglot-reconnect)
  ("C-c C-e C-s C-s" . eglot-shutdown)
  ("C-c C-e C-s C-a" . eglot-shutdown-all))
  :config
  (setq-default eglot-workspace-configuration
        '((haskell (plugin (stan (globalOn . :json-false))))))
  (setq eglot-confirm-server-initiated-edits nil))

(use-package eglot-x
  :straight (eglot-x
	     :type git
	     :host github
	     :repo "nemethf/eglot-x"
	     :files ("*.el"))
  :config
  :after rustic
  (eglot-x-setup))

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

(use-package hl-todo
  :straight t
  :demand t
  :init
  (global-hl-todo-mode))

(use-package vertico
  :straight t
  :demand t
  :bind (:map vertico-map
            ("C-n" . vertico-next)
            ("C-p" . vertico-previous))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
    :straight t
    :init
    (savehist-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package dumb-jump
    :straight t
    :demand t
    :config
    (setq dumb-jump-force-searcher 'ag))

(use-package xref
  :straight t
  :config
  (setq xref-prompt-for-identifier nil))

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(use-package sr-speedbar
  :straight t
  :config
  (speedbar-add-supported-extension ".hs")
  (setq speedbar-use-images nil
        sr-speedbar-right-side nil)
  :bind
  (:map speedbar-mode-map
        ("<TAB>" . speedbar-expand-line)
        ("<backtab>" . speedbar-contract-line)
        ("q" . sr-speedbar-close)
        ("Q" . nil)))

;; (defun sr-speedbar-toggle-and-switch ()
;;   (interactive)
;;   (if (not (equal (current-buffer) sr-speedbar-buffer-name))
;;       (progn
;;         (sr-speedbar-open)
;;         (switch-to-buffer sr-speedbar-buffer-name))
;;     (sr-speedbar-close)))

(use-package cdlatex
    :straight t)

(use-package auctex
    :straight t
    :demand t
    :hook
    (LaTeX-mode . reftex-mode)
    (LaTeX-mode . cdlatex-mode)
    (LaTeX-mode . jinx-mode)
    (LaTeX-mode . display-line-numbers-mode)
    (LaTeX-mode . prettify-symbols-mode)
    (LaTeX-mode . smartparens-mode)
    :bind
    ;; (:map LaTeX-mode-map
    ;; ("<C-m> C-w" . avy-goto-subword-1)
    ;; ("<C-m> <C-m>" . avy-goto-subword-1)
    ;; ("C-S-f" . subword-forward)
    ;; ("C-S-b" . subword-backward)
    ;; ("C-S-k" . subword-kill)
    ;; ("C-S-t" . subword-transpose)
    ;; ("C-<backspace>" . subword-backward-kill))
    :mode
    ("\\.tex\\'" . LaTeX-mode)
    ("\\.mng\\'" . LaTeX-mode)
    ("\\.lhs\\'" . LaTeX-mode))

(use-package citar
  :straight t
  :config
  (setq citar-bibliography '("~/Research/artin.bib"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))

(use-package biblio
  :straight t
  :config
  (require 'biblio-download))

(use-package markdown-mode
  :straight t)

(use-package ott-mode
    :demand t
    :hook
    (ott-mode . smartparens-mode)
    (ott-mode . (lambda () (display-line-numbers-mode 1))))

(with-eval-after-load 'compile
  (push 'ott compilation-error-regexp-alist)
  (push '(ott "File \\([a-zA-Z0-9/\\._-]+\\) on line \\([0-9]+\\).*$" 1 2 nil) compilation-error-regexp-alist-alist))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh")
  :hook
  (term-mode . compilation-shell-minor-mode))

(use-package vterm
  :demand t
  :hook
  (vterm-mode . compilation-shell-minor-mode)
  :bind
  ("s-\\" . vterm)
  ("s-<return>" . vterm-other-window))
(require 'vterm)

(use-package eshell
  :hook
  (eshell-mode . compilation-shell-minor-mode))

(use-package shell :hook (shell-mode . compilation-shell-minor-mode))

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

(use-package olivetti :straight t)

(defun reading-setup ()
   "Sets a fixed width (monospace) font in current buffer"
   (interactive)
   (face-remap-add-relative 'variable-pitch :family "Canela Text"
                                           :height 1.2)
   (text-scale-set 1))

(use-package nov
   :straight t
   :demand t
   :bind
   (:map nov-mode-map
   ("j" . osx-dictionary-search-word-at-point))
   :mode
   (("\\.epub\\'" . nov-mode))
   :hook
   (nov-mode . olivetti-mode)
   (nov-mode . reading-setup))

(use-package pdf-tools
  :demand t
  :bind
  (:map pdf-view-mode-map ("g" . revert-buffer-quick)
                          ("M-s o" . occur)
                          ("<C-m> C-o" . occur)
                          ("o" . pdf-outline)
                          ("M-g M-g" . pdf-view-goto-page))
  :mode
  (("\\.pdf\\'" . pdf-view-mode))
  :config
  (auto-revert-mode 1)
  (setq auto-revert-verbose nil
        pdf-view-use-scaling t)
  :hook
  (pdf-view-mode . pdf-view-midnight-minor-mode))

(use-package djvu :straight t)

(use-package djvu3
  :straight
    (djvu3
     :type git
     :host github
     :repo "dalanicolai/djvu3"
     :files (".el")))

(global-set-key (kbd "C-c C-.") 'eldoc)

(use-package bnf-mode :straight t)

(use-package w3m :straight t)

(require 'haskell-config)

(require 'agda2-config)

(require 'idris2-config)

(require 'emacs-lisp-config)

(require 'racket-config)

(require 'common-lisp-config)

(require 'ocaml-config)

(require 'lean-config)

(require 'nix-config)

(require 'rust-config)

(require 'web-langs-config)

(use-package python-mode)

(use-package swift-mode
    :straight t)

(use-package pass
  :straight t)

(use-package epa
  :straight t)

(require 'serialization-stuff)

(require 'writing-utils)
(require 'mail-config)
(require 'telega-config)
(require 'elfeed-config)

(use-package ement
    :straight t
    :config
    (setq ement-auto-sync nil))

(use-package rmsbolt
  :straight t)

(use-package prodigy
  :straight t)

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


(use-package biome :straight t)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs loaded in %s with %d garbage collections."
		     (format "%.3f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)
	    (setq emacs-started t)))
