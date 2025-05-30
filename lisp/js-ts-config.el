;;; js-ts-config.el --- Description -*- lexical-binding: t; -*-

(use-package npm-mode
  :straight t)

(use-package jsdoc :straight t)

(use-package nodejs-repl :straight t)

;; (use-package typescript-ts-mode
;;   :straight t
;;   :init
;;   (add-to-list 'treesit-language-source-alist
;;                '(typescript "https://github.com/tree-sitter/tree-sitter-typescript"))
;;   (treesit-install-language-grammar typescript))

(provide 'js-ts-config)
