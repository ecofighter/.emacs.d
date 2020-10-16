;;; 20-ddskk.el -- skk japanese input method; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'ddskk)

(global-set-key (kbd "C-x j") 'skk-mode)
(global-set-key (kbd "C-x J") 'skk-auto-fill-mode)

;; (setq-default skk-kutouten-type '("．" . "，"))
(setq-default skk-use-azik t)
;; (setup "skk-study")
;; (setq-default skk-user-directory "~/.skk-jisyo")
(setq-default skk-server-host "127.0.0.1")
(setq-default skk-server-portnum 1178)

;; (setup-after "skk"
;;   (setup "context-skk")
;;   (add-hook 'skk-load-hook #'(lambda ()
;;                                (context-skk-mode 1))))

;; (setup-lazy '(context-skk-mode
;;               context-skk-context-check
;;               context-skk-insert
;;               context-skk-customize) "context-skk")

(provide '20-ddskk)
;;; 20-ddskk.el ends here
