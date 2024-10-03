;;; init.el --- Description -*- lexical-binding: t; -*-

(setq-default message-log-max :error)

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

(setq-default warning-minimum-level :error)
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil)

(save-place-mode 1)

(require 'ibuffer)

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer)

(use-package discover-my-major
  :straight t
  :bind
  ("C-h <C-m> . discover-my-major")
  ("C-h M-m" . discover-my-mode))

(provide 'buffer-config)