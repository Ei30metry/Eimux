(use-package magit
  :straight t
  :demand t)

(use-package diff-hl :straight t)

(use-package magit-delta :straight t)

(use-package magit-pre-commit
  :straight (:host github
             :repo "DamianB-BitFlipper/magit-pre-commit.el")
  :after magit)

(use-package magit-todos
  :straight t
  :after magit
  :config
  (magit-todos-mode 1))

(use-package git-messenger :straight t)

(use-package browse-at-remote :straight t)

(use-package git-timemachine :straight t)

(use-package git-undo :straight t)

(use-package git-modes :straight t)

(use-package forge
  :straight t)

(provide 'vc-config)
