(install-when-compile 'org)
(install-when-compile 'org-evil)

(setup-after "org"
  (plist-put org-format-latex-options :scale 2.0)
  (setup-after "evil"
    (setup "org-evil")))

(provide-file)