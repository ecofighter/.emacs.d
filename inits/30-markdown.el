;;; 30-markdown -- markdown; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(with-eval-after-load "markdown-mode"
  (setq-default markdown-command "pandoc -f markdown -t html5 --template=GitHub.html5 -s --self-contained")
  (setq-default markdown-open-command "firefox --new-window"))

(provide '30-markdown)
;;; 30-markdown.el ends here
