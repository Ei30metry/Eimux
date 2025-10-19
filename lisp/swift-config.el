;; -*- lexical-binding: t; -*-

;; https://codeberg.org/woolsweater/.emacs.d/src/branch/main/modules/my-swift-mode.el
(require 'cl-lib)
(require 'swift-lsp)
(require 'apple-docs-query)
(require 'xcode-build)
(require 'swift-refactor)

(use-package swift-mode
  :straight t)

(use-package swift-helpful :straight t)

;; (use-package flycheck-swift :straight t)

(use-package swift-format :straight t)

;;; swift-config.el ends here
(provide 'swift-config)
