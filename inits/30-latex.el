(install-when-compile 'auctex)
(install-when-compile 'auctex-latexmk)
(install-when-compile 'company-auctex)
(install-when-compile 'company-reftex)
(install-when-compile 'company-math)
(install-when-compile 'latex-math-preview)

(with-eval-after-load "latex-math-preview"
  (setq latex-math-preview-command-path-alist
        '((platex . "uplatex")
          (pdflatex . "lualatex")
          (dvipng . "dvipng")
          (dvips . "dvips")
          (gs . "gs")))
  (setq latex-math-preview-tex-to-png-for-preview '(platex dvipng))
  (setq latex-math-preview-tex-to-png-for-save '(platex dvipng))
  (setq latex-math-preview-tex-to-eps-for-save '(platex dvips-to-eps))
  (setq latex-math-preview-tex-to-ps-for-save '(platex dvips-to-ps))
  (setq latex-math-preview-beamer-to-png '(platex dvipng))
  (setq latex-math-preview-convert-dvipng-color-mode t)
  (setq latex-math-preview-select-preview-window nil))

(setup-expecting "tex-site"
  (setup-expecting "auctex-latexmk"
    (add-hook 'LaTeX-mode-hook #'auctex-latexmk-setup))
  (add-hook 'LaTeX-mode-hook #'flycheck-mode-on-safe)
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook #'reftex-mode)
  (setq-default reftex-plug-into-AUCTeX t)
  (with-eval-after-load "reftex"
    (add-hook 'reftex-mode-hook
              #'(lambda ()
                  (add-to-list 'company-backends 'company-reftex-citations)
                  (add-to-list 'company-backends 'company-reftex-labels))))
  (add-hook 'LaTeX-mode-hook #'(lambda () (TeX-engine-set "luatex")))
  (setup-after "company"
    (add-hook 'LaTeX-mode-hook #'company-auctex-init))
  (defun build-with-latexmk ()
    (interactive)
    (TeX-save-document (TeX-master-file))
    (TeX-command "LatexMk" 'TeX-master-file -1))
  (dolist (mode '(latex-mode tex-mode))
    (evil-leader/set-key-for-mode mode
      "m b" 'build-with-latexmk
      "m p" 'latex-math-preview-expression)))

(setup-after "tex"
  (setq TeX-view-program-selection '((output-pdf "Zathura"))))

(setup-after "auctex-latexmk"
  (setq auctex-latexmk-inherit-TeX-PDF-mode nil))

;; (setup-after "latex-math-preview"
;;   (setq latex-math-preview-command-path-alist
;;         '((latex . "dvilualatex")
;;           (pdflatex . "lualatex")
;;           (dvipng . "dvipng")
;;           (dvips . "dvips")
;;           (gs . "gs")))
;;   (setq latex-math-preview-tex-to-png-for-preview '(latex dvipng))
;;   (setq latex-math-preview-tex-to-png-for-save '(latex dvipng))
;;   (setq latex-math-preview-tex-to-eps-for-save '(latex dvips-to-eps))
;;   (setq latex-math-preview-tex-to-ps-for-save '(latex dvips-to-ps))
;;   (setq latex-math-preview-beamer-to-png '(latex dvipng))
;;   (setq latex-math-preview-convert-dvipng-color-mode t))

(provide-file)
