(setq ediff-keep-variants nil
      ediff-make-buffers-readonly-at-startup nil
      ediff-merge-revisions-with-ancestor t
      ediff-show-clashes-only t
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'diffing-config)
