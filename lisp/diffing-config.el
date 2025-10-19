(setq ediff-keep-variants nil
      ediff-make-buffers-readonly-at-startup nil
      ediff-merge-revisions-with-ancestor t
      ediff-show-clashes-only t
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(setq ediff-highlight-all-diffs nil)

(custom-set-faces
 '(ediff-current-diff-A ((t (:inherit diff-removed))))
 '(ediff-current-diff-B ((t (:inherit diff-added))))
 '(ediff-current-diff-C ((t (:inherit diff-changed))))


 '(ediff-fine-diff-A ((t (:inherit diff-refine-removed))))
 '(ediff-fine-diff-B ((t (:inherit diff-refine-added))))
 '(ediff-fine-diff-C ((t (:inherit diff-refine-change)))))


(use-package difftastic
  :straight t)

;; (use-package difftastic-bindings
;;   :config (difftastic-bindings-mode))

(provide 'diffing-config)
