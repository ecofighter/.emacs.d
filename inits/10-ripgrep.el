;;; 10-ripgrep.el -- work with ripgrep; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'rg)

(eval-after-load "evil-leader"
  (evil-leader/set-key
    "g r" 'rgrep
    "g p" 'rg-project
    "g d" 'rg-dwim-project-dir
    "g l" 'rg-list-searches))

(provide '10-ripgrep)
;;; 10-ripgrep.el ends here
