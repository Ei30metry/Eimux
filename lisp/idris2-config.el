;;; idris2-config.el --- Description -*- lexical-binding: t; -*-

(use-package idris2-mode
  :straight (idris2-mode
	     :type git
	     :host github
	     :repo "idris-community/idris2-mode"
	     :files ("*.el" "data"))
  :commands (idris2-mode))

(provide 'idris2-config)
