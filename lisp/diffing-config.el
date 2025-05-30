(setq ediff-keep-variants nil
      ediff-make-buffers-readonly-at-startup nil
      ediff-merge-revisions-with-ancestor t
      ediff-show-clashes-only t
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(with-eval-after-load 'ediff
       (set-face-foreground
        ediff-current-diff-face-B "green")
       (make-face-italic ediff-current-diff-face-B))

(provide 'diffing-config)
