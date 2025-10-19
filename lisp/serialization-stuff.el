;;; init.el --- Description -*- lexical-binding: t; -*-

(use-package json-mode
  :straight t)

(use-package yaml-mode
   :straight t)

(use-package csv-mode
  :straight t)

(use-package grid-table
  :straight (grid-table
             :type git
             :host github
             :repo "yibie/grid-table"
             :files ("*.el")))

(provide 'serialization-stuff)
