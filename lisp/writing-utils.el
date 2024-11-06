;;; init.el --- Description -*- lexical-binding: t; -*-

(require 'jinx)

(define-key jinx-mode-map (kbd "<C-m> C-j C-n") 'jinx-next)
(define-key jinx-mode-map (kbd "<C-m> C-j C-p") 'jinx-previous)

(use-package powerthesaurus
  :straight t
  :bind
  ("M-^" . powerthesaurus-lookup-dwim))

(use-package synosaurus :straight t)

(use-package reverso :straight t)

(use-package osx-dictionary
  :straight t
  :bind
  ("M-#" . osx-dictionary-search-word-at-point)
  (:map osx-dictionary-mode-map
        ("k" . osx-dictionary-quit)
        ("j" . osx-dictionary-search-word-at-point)))

(provide 'writing-utils)
