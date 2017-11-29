(add-to-list 'default-frame-alist '(font . "Ricty-12.0" ))
(set-face-attribute 'default t :font "Ricty-12.0" )

(setq inhibit-startup-screen t)

(install-when-compile 'gruvbox-theme)

(require 'gruvbox-theme)
(load-theme 'gruvbox-dark-medium t)

(require 'el-init)
(el-init-provide)
