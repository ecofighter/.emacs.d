(install-when-compile 'smart-mode-line)

(setup-include "smart-mode-line"
  (column-number-mode 1)
  (setq sml/name-width '(5 . 25))
  (setq sml/mode-width 'full)
  (setq sml/hidden-modes '(" Server" " WK" " ivy" " fd" " Undo-Tree" " WS" " SP" " company"))
  (setq evil-insert-state-tag (propertize " <I> " 'face '((:background "#076678"))))
  (setq evil-visual-state-tag (propertize " <V> " 'face '((:background "#fe8019"))))
  (setq evil-replace-state-tag (propertize " <R> " 'face '((:background "#8f3f71"))))
  (setq evil-mode-line-format '(before . mode-line-front-space))
  (sml/setup))

(provide-file)
