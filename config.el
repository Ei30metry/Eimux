(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(use-package evil
  :ensure t ;; install evil if not installed
  :config
  (setq evil-auto-indent nil)
  (setq evil-ex-sibstitute-global t)
  (setq evil-move-cursot-back nil)
  (setq evil-move-beyong-eol t)
  (setq evil-kill-on-visual-paste nil)
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration nil)
  (evil-mode))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init
   '(pass
     calendar
     dired
     pdf
     company
     vterm
     flycheck
     xref
     eshell
     org
     magit
     git-timemachine)))

(add-to-list 'load-path "~/.config/haskmacs/evil-commentary")
(require 'evil-commentary)
(evil-commentary-mode)

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-quickscope
  :after evil
  :config
  :hook ((prog-mode . turn-on-evil-quickscope-mode)
	 (LaTeX-mode . turn-on-evil-quickscope-mode)
	 (org-mode . turn-on-evil-quickscope-mode)))

(use-package evil-lion
  :ensure t
  :init
  (evil-lion-mode))

(winner-mode 1)

(setq-default message-log-max nil)

(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
            (kill-buffer buffer)))))

(setq-default warning-minimum-level nil)
(setq make-backup-files nil)

(use-package general
  :ensure t
  :config
  (general-evil-setup t))


(defun +evil--window-swap (direction)
  "Move current window to the next window in DIRECTION.
If there are no windows there and there is only one window, split in that
direction and place this window there. If there are no windows and this isn't
the only window, use evil-window-move-* (e.g. `evil-window-move-far-left')."
  (when (window-dedicated-p)
    (user-error "Cannot swap a dedicated window"))
  (let* ((this-window (selected-window))
         (this-buffer (current-buffer))
         (that-window (windmove-find-other-window direction nil this-window))
         (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
              (window-dedicated-p this-window))
      (setq that-buffer nil that-window nil))
    (if (not (or that-window (one-window-p t)))
        (funcall (pcase direction
                   ('left  #'evil-window-move-far-left)
                   ('right #'evil-window-move-far-right)
                   ('up    #'evil-window-move-very-top)
                   ('down  #'evil-window-move-very-bottom)))
      (unless that-window
        (setq that-window
              (split-window this-window nil
                            (pcase direction
                              ('up 'above)
                              ('down 'below)
                              (_ direction))))
        (with-selected-window that-window
          (switch-to-buffer (doom-fallback-buffer)))
        (setq that-buffer (window-buffer that-window)))
      (window-swap-states this-window that-window)
      (select-window that-window))))

(defun +evil/window-move-left ()
  "Swap windows to the left."
  (interactive) (+evil--window-swap 'left))
;;;###autoload
(defun +evil/window-move-right ()
  "Swap windows to the right"
  (interactive) (+evil--window-swap 'right))
;;;###autoload
(defun +evil/window-move-up ()
  "Swap windows upward."
  (interactive) (+evil--window-swap 'up))
;;;###autoload
(defun +evil/window-move-down ()
  "Swap windows downward."
  (interactive) (+evil--window-swap 'down))

(defun doom/window-maximize-buffer (&optional arg)
  "Close other windows to focus on this one.
Use `winner-undo' to undo this. Alternatively, use `doom/window-enlargen'."
  (interactive "P")
  (when (and (bound-and-true-p +popup-mode)
             (+popup-window-p))
    (+popup/raise (selected-window)))
  (delete-other-windows))

(defvar winner-undone-data  nil) ; There confs have been passed.

(defun winner-undo ()
  "Switch back to an earlier window configuration saved by Winner mode.
In other words, \"undo\" changes in window configuration."
  (interactive)
  (cond
   ((not winner-mode) (error "Winner mode is turned off"))
   (t (unless (and (eq last-command 'winner-undo)
 		   (eq winner-undo-frame (selected-frame)))
	(winner-save-conditionally)     ; current configuration->stack
 	(setq winner-undo-frame (selected-frame))
 	(setq winner-point-alist (winner-make-point-alist))
 	(setq winner-pending-undo-ring (winner-ring (selected-frame)))
 	(setq winner-undo-counter 0)
 	(setq winner-undone-data (list (winner-win-data))))
      (cl-incf winner-undo-counter)	; starting at 1
      (when (and (winner-undo-this)
 		 (not (window-minibuffer-p)))
 	(message "Winner undo (%d / %d)"
 		 winner-undo-counter
 		 (1- (ring-length winner-pending-undo-ring)))))))



(nvmap :prefix "SPC"
       "." '(find-file :which-key "Find file")
       ":" '(execute-extended-command :which-key "M-x")

       ;; Buffers
       "b b" '(ibuffer :which-key "Ibufer")
       ","   '(persp-switch-to-buffer :which-key "Show buffers")
       "b k" '(kill-current-buffer :which-key "Kill current buffer")
       "b ]" '(next-buffer :which-key "Next buffer")
       "b [" '(previous-buffer :which-key "Previous buffer")
       "b B" '(ibuffer-list-buffers :which-key "Ibuffer list buffers")
       "b K" '(kill-buffer :which-key "kill all buffers")

       ;; Eshell
       "e h" '(counsel-esh-history :which-key "Eshell history")
       "e s" '(eshell :which-key "Eshell")
       "f r" '(counsel-recentf :which-key "Recent files")
       "h r r" '((lambda () (interactive) (load-file "~/.config/my-emacs/init.el")) :which-key "Reload emacs config")
       "t t" '(toggle-truncate-lines :which-key "Toggle truncate lines")

       ;; Window splits
       "w d" '(evil-window-delete :which-key "Close window")
       "w n" '(evil-window-new :which-key "New window")
       "w s" '(evil-window-split :which-key "Horizontal split window")
       "w v" '(evil-window vsplit :which-key "Vertical split window")

       ;; Window motions
       "w h" '(evil-window-left :which-key "Window left")
       "w l" '(evil-window-right :which-key "Window right")
       "w k" '(evil-window-up :which-key "Window up")
       "w j" '(evil-widnow-down :which-key "Window down")
       "w w" '(evil-window-next :which-key "Next Window")
       "w H" '(+evil/window-move-left :which-key "Move window to left")
       "w L" '(+evil/window-move-right :which-key "Move window to right")
       "w J" '(+evil/window-move-down :which-key "Move window to down")
       "w K" '(+evil/window-move-up :which-key "Move window to up")

       ;; Window size

       "w m m" '(doom/window-maximize-buffer :which-key "Full screen window")
       "w u" '(winner-undo :whic-key "Revert back to the last window state")

       ;; Company-mode

       ;; Magit

       "g g" '(magit-status :which-key "Magit status")

       ;; Haskell-mode
       "o r" '(haskell-session-change :which-key "Open Haskell REPL")

       ;; Terminal
       "o t" '(vterm :which-key "Open vterm")
       )

;; (nvmap
;;        "-SPC" '(+company/complete :which-key "bring up the pop up menu for autocomplete"))

(setq mac-option-key-is-meta t
      mac-command-key-is-meta nil
      mac-command-modifier 'none
      mac-option-modifier 'meta)

(use-package osx-lib
  :ensure t)

(use-package osx-plist
  :ensure t)

(setq confirm-kill-emacs 'y-or-n-p)

(add-to-list 'custom-theme-load-path (expand-file-name "~/.config/haskmacs/themes/"))
;; (load-theme 'nord t)

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode))

(set-face-attribute 'default nil'
                    :font "Andale Mono 14"
                    :weight 'medium)

(set-face-attribute 'variable-pitch nil
                    :font "Andale Mono 14"
                    :weight 'medium)

(set-face-attribute 'fixed-pitch nil
                    :font "Andale Mono 14"
                    :weight 'medium)
;; needed for emacsclient
(add-to-list 'default-frame-alist '(font . "Andale Mono 14"))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "It's good to have an end to journey toward; but it's the journey that matters in the end.")
  (setq dashboard-startup-banner "~/.config/haskmacs/images/lambda.png")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 10)
                          (agenda . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; (setq fancy-splash-image "~/.config/my-emacs/images/lambda.png")

;; for emacs 29
;; (setq frame-resize-pixelwise t)
;; (add-to-list 'default-frame-alist '(undecorated . t))
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)
(setq display-line-numbers-type 'relative)

(use-package org
  :ensure t
  :defer t
  :init
  (setq org-directory (expand-file-name "~/Journal"))
  (unless (file-exists-p org-directory)
    (mkdir org-directory t))
  :config
  (setq org-startup-indented t)
  (setq org-return-follows-link t)
  (setq org-src-tab-acts-natively nil)
  (add-hook 'org-mode-hook 'smartparens-mode)
  (add-hook 'org-agenda-mode-hook
	    (lambda ()
	      (visual-line-mode -1)
	      (toggle-truncate-lines 1)
	      (display-line-numbers-mode 0)))
  (add-hook 'org-mode-hook
	    (lambda ()
	      (rainbow-delimiters-mode -1))))

(use-package org-contrib
  :after (org)
  :config
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme '(navigation insert textobjects additional calendar todo))))
  (add-to-list 'evil-emacs-state-modes 'org-agenda-mode)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-bullets
   :ensure t)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sq" . "src sql")))

(use-package org-super-agenda
   :ensure t)

(add-to-list 'load-path "~/.config/haskmacs/org-journal")
(require 'org-journal)

;; (add-to-list 'load-path "~/.config/haskmacs/helm-bibtex")
;; (autoload 'helm-bibtex "helm-bibtex" "" t)

(add-to-list 'load-path "~/.config/haskmacs/org-ql")
(add-to-list 'load-path "~/.config/haskmacs/peg")
(require 'org-ql)

(use-package which-key
  :ensure t
  :config
  (setq which-key-allow-imprecise-window-fit t)
  :init
  (which-key-mode))

(setq which-key-idle-delay 0.2)

(use-package persp-mode
  :ensure t)

(add-to-list 'load-path "~/.config/haskmacs/rainbow-delimiters")
(require 'rainbow-delimiters)
(add-hook 'lisp-mode #'rainbow-delimiters-mode)

(add-to-list 'load-path "~/.config/haskmacs/treemacs")
(require 'treemacs-mode)

(use-package magit
  :ensure t)

(use-package eglot
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook (
         (typescript-mode . lsp)
         (json-mode . lsp)
         (haskell-mode . lsp)
         ;;(python-mode . lsp)
         )
  :commands lsp
  :init
  (setq lsp-keymap-prefix nil)
  :config
  (setq lsp-idle-delay 1))

(use-package lsp-ui
  :ensure t)

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode))

(use-package vertico
  :ensure t
  :bind (:map vertico-map
            ("C-j" . vertico-next)
            ("C-k" . vertico-previous))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.15)
  (setq company-minimum-prefix-length 2)
  (setq company-show-number t))

(add-hook 'after-init-hook 'global-company-mode)
;;;###autoload
(defun +company-has-completion-p ()
  "Return non-nil if a completion candidate exists at point."
  (when company-mode
    (unless company-candidates-length
      (company-manual-begin))
    (= company-candidates-length 1)))

;;;###autoload
(defun +company/toggle-auto-completion ()
  "Toggle as-you-type code completion."
  (interactive)
  (require 'company)
  (setq company-idle-delay (unless company-idle-delay 0.2))
  (message "Auto completion %s"
           (if company-idle-delay "enabled" "disabled")))

;;;###autoload
(defun +company/complete ()
  "Bring up the completion popup. If only one result, complete it."
  (interactive)
  (require 'company)
  (when (ignore-errors
          (/= (point)
              (cdr (bounds-of-thing-at-point 'symbol))))
    (save-excursion (insert " ")))
  (when (and (company-manual-begin)
             (= company-candidates-length 1))
    (company-complete-common)))

;;;###autoload
(defun +company/dabbrev ()
  "Invokes `company-dabbrev-code' in prog-mode buffers and `company-dabbrev'
everywhere else."
  (interactive)
  (call-interactively
   (if (derived-mode-p 'prog-mode)
       #'company-dabbrev-code
     #'company-dabbrev)))

(use-package company-box
  :ensure t)

(use-package vterm
  :ensure t)

(use-package vterm-toggle
  :ensure t)

;; (use-package projectile
;;   :ensure t)

(use-package treemacs
  :ensure t)

(use-package xref
  :ensure t)

(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 4)
(setq-default tab-width 4)
(setq-default evil-indent-convert-tabs nil)
(setq-default indent-tabs-mode nil)
(setq-default evil-shift-round nil)

(use-package pdf-tools
  :ensure t)

(use-package haskell-mode
  :ensure t)

(add-hook 'haskell-mode-hook #'lsp-mode)
(add-hook 'haskell-mode-hook (lambda () (setq evil-auto-indent nil)))
(custom-set-variables '(haskell-stylish-on-save t))

;; (defun dotspacemacs/user-config ()
;;  (with-eval-after-load "haskell-mode"
;;     ;; This changes the evil "O" and "o" keys for haskell-mode to make sure that
;;     ;; indentation is done correctly. See
;;     ;; https://github.com/haskell/haskell-mode/issues/1265#issuecomment-252492026.
;;     (defun haskell-evil-open-above ()
;;       (interactive)
;;       (evil-digit-argument-or-evil-beginning-of-line)
;;       (haskell-indentation-newline-and-indent)
;;       (evil-previous-line)
;;       (haskell-indentation-indent-line)
;;       (evil-append-line nil))

;;     (defun haskell-evil-open-below ()
;;       (interactive)
;;       (evil-append-line nil)
;;       (haskell-indentation-newline-and-indent))

;;     (evil-define-key 'normal haskell-mode-map
;;       "o" 'haskell-evil-open-below
;;       "O" 'haskell-evil-open-above)
;;   )
;; )

(use-package lsp-haskell
  :ensure t
  :after haskell-mode
  :config
  (setq lsp-haskell-server-path "haskell-language-server-9.4.2\~1.8.0.0"
        lsp-haskell-liquid-on t
        lsp-haskell-fomatting-provider "stylish-haskell"))

(setq haskell-font-lock-symbols t)

(add-to-list 'load-path "~/.config/haskmacs/ghcid")
(require 'ghcid)

;; (use-package python-mode
;;   :ensure t)

(use-package json-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package tex-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)
