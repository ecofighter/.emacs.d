;;; 20-fcitx.el -- fcitx integration
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'fcitx)

(add-hook 'after-init-hook #'fcitx-default-setup)
(setq-default fcitx-use-dbus t)

(provide '20-fcitx)
;;; 20-fcitx.el ends here
