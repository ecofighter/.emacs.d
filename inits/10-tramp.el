;;; 10-tramp.el -- tramp config; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)

(with-eval-after-load 'tramp
  (pcase system-type
    ('windows-nt
     (setq tramp-default-method 'scpx))
    (_
     (setq tramp-default-method 'scp)))
  (add-to-list 'tramp-remote-path "~/.ghcup/bin"))

(provide '10-tramp)
;;; 10-tramp.el ends here
