;;; init.el --- Description -*- lexical-binding: t; -*-

(use-package nix-mode
   :straight t)

(defun darwin-rebuild-switch () ())
(defun nix-channel-update () ())
(defun nix-collect-garbage-d () ())

(global-set-key (kbd "C-x C-. C-n o") #'(lambda () (interactive) (find-file "~/.config/nix-darwin/flake.nix")))
(global-set-key (kbd "C-x C-. C-n r") #'darwin-rebuild-switch)
(global-set-key (kbd "C-x C-. C-n u") #'nix-channel-update)
(global-set-key (kbd "C-x C-. C-n d") #'nix-collect-garbage-d)

(provide 'nix-config)
