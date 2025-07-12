;;; window-frame-config.el --- Description -*- lexical-binding: t; -*-

(setq confirm-kill-emacs 'y-or-n-p)
(set-frame-parameter nil 'internal-border-width 0)

(defun tab-switch-no-tab-bar (name)
  "Doesn't turn tab-bar-mode on when switching to a new tab"
  (interactive
   (let* ((recent-tabs (mapcar (lambda (tab)
                                 (alist-get 'name tab))
                               (tab-bar--tabs-recent))))
     (list (completing-read (format-prompt "Switch to tab by name"
                                           (car recent-tabs))
                            recent-tabs nil nil nil nil recent-tabs))))
  (tab-bar-switch-to-tab name)
  (if tab-bar-mode
      (tab-bar-mode -1)))

;; TODO: Set C-x C-, as the prefix for tab-bar commands
(use-package tab-bar-mode
  :bind
  ("C-x C-, C-," . tab-switch-no-tab-bar)
  ("C-x C-, s"   . tab-switch-no-tab-bar)
  ("C-x C-, C-." . tab-recent)

  ("C-x C-, C-s" . desktop-save)
  ("C-x C-, C-l" . desktop-read)

  ("C-x C-, r"   . tab-rename)
  ("C-x C-, k"   . tab-close)
  ("C-x C-, K"   . tab-close-other)
  ("C-x C-, e"   . tab-bar-echo-area-print-tab-name)

  ("C-x C-, n"   . tab-next)
  ("s-]"         . tab-next)

  ("C-x C-, p"   . tab-previous)
  ("s-["         . tab-previous))

(use-package activities
  :straight
  (activities
   :type git
   :host github
   :repo "alphapapa/activities.el"
   :files ("*.el")))

(use-package tab-bar-echo-area
  :straight t
  :config
  (tab-bar-echo-area-mode 1))

(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-.") 'consult-buffer)

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
          "\\*Embark \\(Export\\|Collect\\):.*\\*"
          "\\*eldoc for\\*$"
          "Output\\*$"
          "\\*Backtrace\\*"
          "\\*Async Shell Command\\*"
          "\\*sly-\\(macroexpansion\\|description\\|db for*\\)\\*"
          inferior-python-mode
          help-mode
          compilation-mode
          haskell-interactive-mode
          comint-mode
          vterm-mode
          eat-mode
          pdf-outline-buffer-mode
          helpful-mode
          osx-dictionary-mode
          racket-repl-mode
          racket-describe-mode
          nix-repl-mode
          idris2-repl-mode
          idris2-info-mode
          idris2-compiler-notes-mode
          sly-mrepl-mode
          inferior-emacs-lisp-mode
          sly-db-mode
          term-mode
          eshell-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

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

(use-package show-font :straight t)

(defun ask-before-closing ()
  "Close only if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Really close frame? "))
      (save-buffers-kill-emacs)
    (message "Canceled frame close")))

(when (daemonp)
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

(use-package ace-window
  :straight t
  :demand t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-dispatch-always t)
  :bind
  ("M-o" . ace-window)
  ("C-x o" . other-window))

(use-package ultra-scroll
  :straight
  (ultra-scroll
    :type git
    :host github
    :repo "jdtsmith/ultra-scroll"
    :files ("*.el"))
  :config
  (ultra-scroll-mode 1))

(provide 'window-frame-config)
