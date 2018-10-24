(install-when-compile 'auctex)
(install-when-compile 'auctex-latexmk)
(install-when-compile 'company-auctex)

(setup-expecting "auctex"
  (setup-expecting "auctex-latexmk"
    (add-hook 'LaTeX-mode-hook #'auctex-latexmk-setup))
  (add-hook 'LaTeX-mode-hook #'flycheck-mode-on-safe)
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook #'(lambda () (TeX-engine-set "luatex")))
  (add-hook 'LaTeX-mode-hook #'(lambda () (setup-after "company"
                                            (company-auctex-init)))))


(setup-after "auctex"
  (setq-default TeX-view-program-selection '((output-pdf "Zathura")))
  (defun build-with-latexmk ()
    (interactive)
    (TeX-save-document (TeX-master-file))
    (TeX-command "LatexMk" 'TeX-master-file -1))
  (dolist (mode '(latex-mode tex-mode))
    (evil-leader/set-key-for-mode mode
      "m b" 'build-with-latexmk)))

(setup-after "auctex-latexmk"
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(provide-file)
