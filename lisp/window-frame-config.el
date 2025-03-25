;;; window-frame-config.el --- Description -*- lexical-binding: t; -*-

(setq confirm-kill-emacs 'y-or-n-p)
(set-frame-parameter nil 'internal-border-width 0)

(use-package tab-bar-mode
  :bind
  ("C-x C-, C-," . tab-switch)
  ("C-x C-, C-k" . tab-close)
  ("C-x C-, C-K" . tab-close-other)

  ("C-x C-, C-n" . tab-next)
  ("s-]"         . tab-next)

  ("C-x C-, C-p" . tab-previous)
  ("s-["         . tab-previous)
  :config
  (tab-bar-mode 1)
  (setq tab-bar-show nil))

(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-.") 'consult-buffer)

;; (use-package perspective
;;   :straight t
;;   :custom
;;   (persp-mode-prefix-key (kbd "C-x C-,"))
;;   :bind
;;   ("C-x k" . (lambda () (interactive) (persp-kill-buffer* nil)))
;;   ("C-x C-, C-," . persp-switch)
;;   ("C-x C-, C-." . persp-switch-last)
;;   ("C-x K" . persp-kill-buffer*)
;;   ("C-." . persp-switch-to-buffer*)
;;   ("C-x b" . switch-to-buffer)
;;   :init
;;   (setq persp-initial-frame-name "misc")
;;   (persp-mode))

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
          "\\*sly-\\(macroexpansion\\|descrition\\)\\*"
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

(use-package show-font :straigh t)

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
        ("C-x o" . other-window)
        ("M-o" . ace-window))

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
