;; -*- lexical-binding: t; -*-

(use-package swift-mode
  :straight t)

(use-package swift-helpful :straight t)

(use-package swift-format :straight t)

(use-package swift-development
  :straight (swift-development
             :type git
             :host github
             :repo "konrad1977/swift-development"
             :files ("*.el"))
  :config
  (require 'swift-development)
  (require 'xcode-project)
  (require 'xcode-build-config)

  (require 'ios-simulator)
  (require 'ios-device)
  (require 'swift-refactor)
  (require 'localizeable-mode))

;;; swift-config.el ends here
(provide 'swift-config)
