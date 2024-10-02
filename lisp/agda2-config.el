;;; agda2-config.el --- Description -*- lexical-binding: t; -*-
(setq agda-mode-directory
      (file-name-directory (substring (shell-command-to-string "agda-mode locate") 0 -1)))

(add-to-list 'load-path agda-mode-directory)

(use-package agda2-mode
  :ensure nil
  :demand t
  :mode (("\\.agda\\'" . agda2-mode)
         ("\\.lagda.md\\'" . agda2-mode))
  :bind
  (:map agda2-mode-map
        ("<C-m> C-w" . avy-goto-subword-1)
        ("<C-m> <C-m>" . avy-goto-subword-1)
        ("C-c h" . agda2-helper-function-type)
        ("C-c C-h" . nil)
        ("C-S-b" . subword-backward)
        ("C-S-d" . subword-kill)
        ("C-S-t" . subword-transpose)
        ("C-<backspace>" . subword-backward-kill)
        ("M-S-<backspace>" . subword-backward-kill)))

(provide 'agda2-config)
