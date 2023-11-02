(straight-use-package 'use-package)

(use-package evil
  :straight t
  :init
  (setq evil-auto-indent nil)
  (setq evil-emacs-state-modes nil)
  (setq evil-motion-state-modes nil)
  (setq evil-ex-sibstitute-global t)
  (setq evil-move-cursot-back nil)
  (setq evil-overriding-maps nil)
  (setq evil-move-beyong-eol t)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :straight t
  :init
  (evil-commentary-mode))
;; (add-to-list 'load-path "~/.config/haskmacs/evil-commentary")
;; (require 'evil-commentary)
;; (evil-commentary-mode)

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-quickscope
  :straight t
  :after evil
  :config
  :hook ((prog-mode . turn-on-evil-quickscope-mode)
	 (LaTeX-mode . turn-on-evil-quickscope-mode)
	 (org-mode . turn-on-evil-quickscope-mode)))

(use-package evil-lion
  :straight t
  :init
  (evil-lion-mode))

(winner-mode 1)

(setq-default message-log-max nil)

(add-hook 'minibuffer-exit-hook
      #'(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
            (kill-buffer buffer)))))

(setq-default warning-minimum-level nil)
(setq make-backup-files nil)
(setq aut-save-default nil)
(setq auto-save-list-file-prefix nil)

;;;###autoload
(require 'windmove)
(defun +company-has-completion-p ()
  "Return non-nil if a completion candidate exists at point."
  (when company-mode
    (unless company-candidates-length
      (company-manual-begin))
    (= company-candidates-length 1)))

(defun kill-all-buffers (&optional buffer-list interactive)
  "Kill all buffers and closes their windows.

If the prefix arg is passed, doesn't close windows and only kill buffers that
belong to the current project."
  (interactive
   (list (if current-prefix-arg
             (doom-project-buffer-list)
           (doom-buffer-list))
         t))
  (if (null buffer-list)
      (message "No buffers to kill")
    (save-some-buffers)
    (delete-other-windows)
    (when (memq (current-buffer) buffer-list)
      (switch-to-buffer (doom-fallback-buffer)))
    (mapc #'kill-buffer buffer-list)
    (doom--message-or-count
     interactive "Killed %d buffers"
     (- (length buffer-list)
        (length (cl-remove-if-not #'buffer-live-p buffer-list))))))

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

     
(defalias 'doom-buffer-list #'buffer-list)
(defvar doom-fallback-buffer-name "*scratch*")

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

(defun doom-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer. See `doom-fallback-buffer-name' to change this."
  (let (buffer-list-update-hook)
    (get-buffer-create doom-fallback-buffer-name)))

 (defun doom--message-or-count (interactive message count)
  (if interactive
      (message message count)
    count))

(defun +evil/window-move-left ()
  "Swap window to the left."
  (interactive) (+evil--window-swap 'left))
;;;###autoload
(defun +evil/window-move-right ()
  "Swap window to the right"
  (interactive) (+evil--window-swap 'right))
;;;###autoload
(defun +evil/window-move-up ()
  "Swap window upward."
  (interactive) (+evil--window-swap 'up))
;;;###autoload
(defun +evil/window-move-down ()
  "Swap window downward."
  (interactive) (+evil--window-swap 'down))

(defun window-maximize-buffer (&optional arg)
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

(use-package general
  :straight t
  :config
  (general-evil-setup t))

(general-create-definer my-leader-def
     :prefix "SPC")

(general-create-definer my-local-leader-def
    :prefix "SPC m")

(my-leader-def
   :states 'normal
   :prefix "SPC p"
   :keymaps '(projectile-mode-map)
   "c" '(projectile-compile-project :which-key "Compile project"))
;; haskell-mode keybindings

(my-leader-def
   :states 'normal
   :prefix "SPC c"
   :keymaps '(lsp-mode-map lsp-ui-mode-map)
   "d" '(lsp-find-definition :which-key "Find definition")
   "t" '(lsp-ui-doc-glance :which-key "Show documentation")
   "r" '(lsp-ui-peek-find-references :which-key "Show documentation")
   "S" '(lsp-mode :which-key "Enable lsp-mode")
   )

(my-local-leader-def
   :states 'normal
   :keymaps '(haskell-mode-map haskell-interactive-mode-map)
   "t" '(haskell-process-do-type :which-key "Show type at point")
   "r" '(haskell-process-reload :which-key "Reload the current module")
   "k" '(haskell-interactive-mode-clear :which-key "Clear the GHCi buffer")
   "l" '(haskell-process-load-file :which-key "Load the module")
   "v" '(haskell-cabal-visit-file :which-key "Open the .cabal file")
   "b" '(haskell-process-cabal-build :which-key "Build the project")
   "x" '(haskell-process-cabal :which-key "Execute a cabal command")
   "s" '(haskell-interactive-switch :which-key "Switch between GHCi and buffer"))

;; agda2-mode keybindings
(my-local-leader-def
   :states 'normal
   :keymaps '(agda2-mode-map agda2-goal-map)
   "a" '(agda2-auto-maybe-all :which-key "Try to solve every goal using Auto")
   "b" '(agda2-previous-goal :which-key "Go to the previous goal")
   "f" '(agda2-next-goal :which-key "Go to the next goal")
   "l" '(agda2-load :which-key "Load the current module")
   "c" '(agda2-make-case :which-key "Case split on the current goal")
   "e" '(agda2-show-context :which-key "Show the context for the current goal")
   "r" '(agda2-refine :which-key "Refine the goal")
   "x q" '(agda2-quit :which-key "Quit")
   "x c" '(agda2-compile :which-key "Compile the project")
   "x r" '(agda2-restart :which-key "Restart agda2-mode")
   "n" '(agda2-compute-normalised-maybe-toplevel :which-key "Show the normalised form")
   "t" '(agda2-goal-type :which-key "Show the type of the goal")
   "SPC" '(agda2-give :which-key "Give input")
   "," '(agda2-goal-and-context :which-key "Show the goal and context")
   "." '(agda2-goal-and-context-and-infered :which-key "Show the goal and context and infered")
   "." '(agda2-goal-and-context-and-checked :which-key "Show the goal and context and checked")
   "=" '(agda2-show-constraints :which-key "Show the constraints")
   "d" '(agda2-goto-definition-keyboard :which-key "Go to defintion")
   "?" '(agda2-show-goals :which-key "Show the goals")
   "RET" '(agda2-elaborate-give :which-key "Elaborate check the give expression")
   )

(my-leader-def
   :keymaps 'normal

   ;; Help menu
   "h f" '(describe-function :which-key "Describe function")
   "h m" '(describe-mode :which-key "Describe mode")
   "h k" '(describe-key :which-key "Describe key")
   "h K" '(describe-keymap :which-key "Describe keymap")
   "h b" '(general-describe-keybindings :which-key "Describe all keybindings")
   "h c" '(describe-char :which-key "Describe char")
   "h x" '(describe-command :which-key "Describe command")
   "h s" '(describe-symbol :which-key "Describe symbol")

   ;; Journal 
   "j N" '(org-journal-new-entry :which-key "New journal entry")
   "j n" '(org-journal-next-entry :which-key "Next journal entry")
   "j p" '(org-journal-previous-entry :which-key "Previous journal entry")
   "j r" '(org-journal-read-entry :which-key "Read journal entry")
   "j s" '(org-journal-search-forever :which-key "Search in all the journal files ")
   "j S" '(org-journal-search :which-key "Search in journal files ")
   

   "h r r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload emacs config")

   "C" '(org-capture :which-key "Org Capture")
   "a" '(org-agenda :which-key "Org Agenda")
   "d" '(dired :which-key "Dired")

   ":" '(execute-extended-command :which-key "M-x")
   "," '(persp-switch-to-buffer :which-key "Show buffers")
   "." '(find-file :which-key "Find file")
   
   ;; Buffers
   "b b" '(ibuffer :which-key "Ibuffer")
   "b k" '(kill-current-buffer :which-key "Kill current buffer")
   "b ]" '(next-buffer :which-key "Next buffer")
   "b [" '(previous-buffer :which-key "Previous buffer")
   "b B" '(ibuffer-list-buffers :which-key "Ibuffer list buffers")
   "b K" '(kill-all-buffers :which-key "kill all buffers")

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
   "w j" '(evil-window-down :which-key "Window down")
   "w w" '(evil-window-next :which-key "Next Window")
   "w H" '(+evil/window-move-left :which-key "Move window to left")
   "w L" '(+evil/window-move-right :which-key "Move window to right")
   "w J" '(+evil/window-move-down :which-key "Move window to down")
   "w K" '(+evil/window-move-up :which-key "Move window to up")

   ;; Window size
   "w m m" '(window-maximize-buffer :which-key "Full screen window")
   "w u" '(winner-undo :which-key "Revert back to the last window state")

   ;; Magit
   "g g" '(magit-status :which-key "Git status")

   ;; "g g" '(magit-status :which-key "Magit status")

   ;; Terminal
   "o t" '(term :which-key "Open term")
   "o e" '(eshell :which-key "Open eshell")

   ;; Searching
   "s i" '(consult-imenu :which-key "Imenu buffer")
   "s I" '(consult-imenu :which-key "Imenu multi-buffer")
   "s r" '(consult-recent-file :which-key "Recent files")

   "/" '(consult-ripgrep :which-key "Search current project"))

(setq mac-option-key-is-meta t
      mac-command-key-is-meta nil
      mac-command-modifier 'super
      mac-option-modifier 'meta)

(use-package osx-lib
  :straight t)

(use-package osx-plist
  :straight t)

(setq confirm-kill-emacs 'y-or-n-p)

(setq scroll-conservatively 101)
;; (use-package reverse-theme
 ;;   :insure t)
 (use-package doom-themes
 :straight t
 :config
 ;; Global settings (defaults)
 (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
       doom-themes-enable-italic t) ; if nil, italics is universally disabled
 (load-theme 'doom-meltbus t)

 ;; Enable flashing mode-line on errors
 (doom-themes-visual-bell-config)
 ;; Enable custom neotree theme (all-the-icons must be installed!)
 (doom-themes-neotree-config)
 ;; Corrects (and improves) org-mode's native fontification.
 (doom-themes-org-config))
 
 (use-package sexy-monochrome-theme :straight t)
 (use-package minimal-theme :straight t)
 (use-package kosmos-theme :straight t)
 ;; (use-package eziam-themes :straight t)
 (use-package almost-mono-themes :straight t)
 (add-to-list 'custom-theme-load-path "~/.config/haskmacs/themes")
 ;; (set-foreground-color "white")
 ;; (set-background-color "black")

 ;; (load-theme 'reverse-theme t)

(use-package doom-modeline
  :straight t
  :config
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-major-mode-color-icon nil)
  :init
  (doom-modeline-mode))

(set-face-attribute 'default nil
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
  :straight t
  :if (display-graphic-p))

(use-package dashboard
:straight t
:init
(setq dashboard-set-heading-icons nil)
(setq dashboard-icon-type 'all-the-icons)
(setq dashboard-set-file-icons t)
(setq dashboard-banner-logo-title "It's good to have an end to journey toward; but it's the journey that matters in the end.")
(setq dashboard-startup-banner "~/.emacs.d/images/lambda.png")
(setq dashboard-center-content t)
(setq dashboard-items '((agenda . 15)))
:config
(dashboard-setup-startup-hook)
(dashboard-modify-heading-icons '((recents . "file-text")
                                  (bookmarks . "book"))))

(menu-bar-mode 1)
 (tool-bar-mode -1)
 (scroll-bar-mode -1)
 (pixel-scroll-precision-mode 1)
 ;; (setq fancy-splash-image "~/.config/my-emacs/images/lambda.png")

 ;; for emacs 29
 ;; (setq frame-resize-pixelwise t)
 ;; (add-to-list 'default-frame-alist '(undecorated . t))
 ;; (global-display-line-numbers-mode 1)
 ;; (defun turn-on-numbers ()
 ;;      (unless (eq major-mode 'pdf-view-mode)
 ;;              (display-line-numbers-mode 1)))

 ;; (type-of turn-on-numbers)
;; (unless (eq major-mode 'pdf-view-mode)
;;         (global-display-line-numbers-mode 1))

 (global-display-line-numbers-mode 1)
 (global-visual-line-mode 1)
 (setq display-line-numbers-type 'relative)

(use-package org
  :straight t
  :init
  (setq org-directory "~/Journal")
  (unless (file-exists-p org-directory)
    (mkdir org-directory t))
  :config
  (setq org-startup-indented t)
  (setq org-log-into-drawer t)
  (setq org-treat-insert-todo-heading-as-state-change t)
  (setq org-return-follows-link t)
  (setq org-src-tab-acts-natively nil)
  (setq org-agenda-files '("~/Agenda/todo.org" "~/Agenda/habits.org"))
  (add-hook 'org-mode-hook 'smartparens-mode)
  (add-hook 'org-agenda-mode-hook
        #'(lambda ()
          (visual-line-mode -1)
          (toggle-truncate-lines 1)
          (display-line-numbers-mode 0))))
  ;; (add-hook 'org-mode-hook
  ;;       (lambda ()
  ;;         (rainbow-delimiters-mode -1))))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Agenda/todo.org" "Inbox")
         "* TODO %?\n  %i\n")))

(use-package org-contrib
  :straight t
  :after (org)
  :config
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(use-package evil-org
  :straight t
  :hook (org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
	    #'(lambda ()
	      (evil-org-set-key-theme '(navigation insert textobjects additional calendar todo))))
  (add-to-list 'evil-emacs-state-modes 'org-agenda-mode)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-bullets
   :straight t)

(add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1)))

(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sq" . "src sql")))

(use-package toc-org
  :straight t
  :config
  (add-hook 'org-mode-hook 'toc-org-mode))

(use-package org-super-agenda
   :straight t)

;; (use-package org-alert
;;   :straight t
;;   :config
;;   (setq alert-default-style 'osx-notifier
;;         org-alert-notification-title "Agenda"
;;         org-alert-interval 300)
;;   :init
;;   (org-alert-enable))

(use-package org-books
  :straight t
  :config
  (setq org-books-file "~/Agenda/books.org"))

(use-package deft
    :straight t
    :config
    (setq deft-directory "~/Journal"
          deft-extensions '("md" "org" "txt")
          deft-recursive t))

(setq deft-directory "~/Journal"
      deft-extensions '("md" "org" "txt")
      deft-recursive t)

(use-package org-journal
    :straight t)

(setq org-journal-date-prefix "#+TITLE: "
      org-journal-dir "~/Journal"
      org-journal-time-prefix "* "
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "%Y-%m-%d.org")



(setq org-directory "~/Journal")



;; (use-package helm-bibtex
;;   :ensure t)

;; (use-package org-ql
;;   :ensure t)
;; (add-to-list 'load-path "~/.config/haskmacs/org-ql")
;; (add-to-list 'load-path "~/.config/haskmacs/peg")
;; (require 'org-ql)

(use-package which-key
  :straight t
  :config
  (setq which-key-allow-imprecise-window-fit t)
  :init
  (which-key-mode))

(setq which-key-idle-delay 0.2)

(use-package persp-mode
  :straight t)

;; (add-to-list 'load-path "~/.config/haskmacs/rainbow-delimiters")
;; (require 'rainbow-delimiters)
;; (use-package rainbow-delimiters
;;   :ensure t)
;; (add-hook 'lisp-mode #'rainbow-delimiters-mode)

(use-package magit
  :straight t)

;; (use-package eglot
;;   :ensure t)

;; for improvement 
(setq read-process-output-max (* 2048 2048))
(setq gc-cons-threshold 100000000)

(use-package lsp-mode
   :straight t
   :commands lsp
   :init
   (setq lsp-keymap-prefix nil)
   :config
   (setq lsp-file-watch-threshold 3000)
   (setq lsp-log-io nil)
   (setq lsp-use-plists nil)
   (setq lsp-idle-delay 1))

(use-package lsp-ui
  :straight t)

(use-package smartparens
  :straight t
  :init
  (smartparens-global-mode))

(use-package vertico
  :straight t
  :bind (:map vertico-map
            ("C-j" . vertico-next)
            ("C-k" . vertico-previous))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :straight t
  :init
  (savehist-mode))

(use-package company
  :straight t
  :config
  (setq company-idle-delay 0.15)
  (setq company-minimum-prefix-length 2)
  (setq company-show-number t))

(add-hook 'after-init-hook 'global-company-mode)

(use-package company-box
  :straight t)

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(add-hook 'term-mode-hook #'(lambda () (display-line-numbers-mode -1)))
(setq explicit-shell-file-name "zsh")

;; (use-package vterm
;;   :straight t
;;   :config
;;   (add-hook 'vterm-mode-hook #'(lambda () (display-line-numbers-mode -1))))

;; (use-package vterm-toggle
;;   :straight t)

;; (use-package eshell
;;   :straight t
;;   (add-hook 'eshell-mode-hook #'(lambda () (display-line-numbers-mode -1))))
(add-hook 'vterm-mode-hook #'(lambda () (display-line-numbers-mode -1)))

(use-package projectile
  :straight t
  :init
  (projectile-mode 1))

(use-package xref
  :straight t)

(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 4)
(setq-default tab-width 4)
(setq-default evil-indent-convert-tabs nil)
(setq-default indent-tabs-mode nil)
(setq-default evil-shift-round nil)

(use-package pdf-tools
  :straight t
  :config
  (add-hook 'pdf-view-mode-hook #'(lambda () (display-line-numbers-mode -1)))
  (pdf-tools-install))

(use-package imenu
  :straight t)

(use-package swiper
  :straight t)

(use-package consult
   :straight t)

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

;; (use-package helpful
;;     :ensure t)

(use-package haskell-mode
  :straight t
  :config
  (setq haskell-font-lock-symbols t)
  (custom-set-variables '(haskell-stylish-on-save nil))
  (custom-set-variables '(haskell-process-log t))
  :hook
  (haskell-mode . (lambda () (setq evil-auto-indent nil)))
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . haskell-auto-insert-module-template)
  (haskell-mode . haskell-decl-scan-mode))
;; (add-hook 'haskell-mode-hook #'lsp-mode)
;;(add-hook 'haskell-mode-hook (lambda () (setq evil-auto-indent nil)))
;; (add-hook 'haskell-mode-hook '(interactive-haskell-mode))
;; (add-hook 'haskell-mode-hook '(haskell-auto-insert-module-template))
;; ;; (add-hook 'haskell-mode-hook '(haskell-decl-scan-mode))
;; (setq haskell-font-lock-symbols t)
;; (custom-set-variables '(haskell-stylish-on-save t))
;; (custom-set-variables '(haskell-process-log t))

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
  :straight t
  :after haskell-mode
  :config
  (setq lsp-haskell-server-path "haskell-language-server-wrapper"))
        ;; lsp-haskell-liquid-on t
        ;; lsp-haskell-fomatting-provider "stylish-haskell"))



(use-package json-mode
  :straight t)

(use-package yaml-mode
  :straight t)

(use-package csv-mode
  :straight t)

(use-package tex-mode
  :straight t)

(add-to-list 'load-path "~/.emacs.d/ott-mode")
(require 'ott-mode)

(use-package markdown-mode
  :straight t)
