;;; early-init.el --- my early-init-file -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(with-eval-after-load 'comp
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-comp-async-jobs-number 4)
  (setq native-comp-speed 3))
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setenv "LSP_USE_PLISTS" "true")
(setq garbage-collection-messages t)

(setq use-default-font-for-symbols t)
;; (set-face-attribute 'default nil :family "Moralerspace Neon" :height 160)
;; (set-fontset-font t 'ascii (font-spec :family "Ricty Diminished" :size 14))
;; (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Ricty Diminished"))
;; (set-fontset-font t 'unicode (font-spec :family "Noto Sans CJK JP") nil 'append)
(let ((table (make-char-table nil)))
  (set-char-table-range table t `(["[-.,:;A-Z_a-z><=!&|+?/\\]+" 0 font-shape-gstring]))
  (set-char-table-parent table composition-function-table)
  (setq composition-function-table table))
;; (set-char-table-range composition-function-table t `(["[,-.;A-Z_a-z]+" 0 font-shape-gstring]))
;; (defun set-buffer-local-composition-table (value)
;;   (let ((table (make-char-table nil)))
;;     (set-char-table-range table t `([,value 0 font-shape-gstring]))
;;     (set-char-table-parent table composition-function-table)
;;     (setq-local composition-function-table table)))
;; (defun set-prog-mode-table ()
;;   (set-buffer-local-composition-table "[-.,:;A-Z_a-z><=!&|+?/\\]+"))
;; (add-hook 'prog-mode-hook #'set-prog-mode-table)
(provide 'early-init)
;;; early-init.el ends here
