;;; js-ts-config.el --- Description -*- lexical-binding: t; -*-

(use-package npm-mode
  :straight t)

(use-package jsdoc :straight t)

(use-package nodejs-repl :straight t)

(use-package typescript-ts-mode
  :straight t
  :init
  (add-to-list 'treesit-language-source-alist
               '(typescript "https://github.com/tree-sitter/tree-sitter-typescript"))
  (unless (treesit-language-available-p 'typescript) (treesit-install-language-grammar typescript)))

(use-package vue-ts-mode
  :straight (vue-ts-mode
             :type git
             :host github
             :repo "8uff3r/vue-ts-mode")
  :init
  (add-to-list 'treesit-language-source-alist '(vue "https://github.com/ikatyang/tree-sitter-vue"))
  (unless (treesit-language-available-p 'vue) (treesit-install-language-grammar vue)))

(provide 'js-ts-config)
