;;; init.el --- Description -*- lexical-binding: t; -*-

(use-package tuareg
  :straight t)

(use-package merlin
  :straight t
  :after tuareg)

(use-package utop
  :straight t)

(provide 'ocaml-config)
