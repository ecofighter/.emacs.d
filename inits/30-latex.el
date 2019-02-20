;;; 30-latex.el -- write latex; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(install-when-compile 'auctex)
(install-when-compile 'auctex-latexmk)
(install-when-compile 'company-auctex)
(install-when-compile 'company-reftex)
(install-when-compile 'company-math)
(install-when-compile 'latex-math-preview)

(with-eval-after-load "latex-math-preview"
  (setq-default latex-math-preview-command-path-alist
                '((platex . "uplatex")
                  (pdflatex . "lualatex")
                  (dvipng . "dvipng")
                  (dvips . "dvips")
                  (gs . "gs")))
  (setq-default latex-math-preview-tex-to-png-for-preview '(platex dvipng))
  (setq-default latex-math-preview-tex-to-png-for-save '(platex dvipng))
  (setq-default latex-math-preview-tex-to-eps-for-save '(platex dvips-to-eps))
  (setq-default latex-math-preview-tex-to-ps-for-save '(platex dvips-to-ps))
  (setq-default latex-math-preview-beamer-to-png '(platex dvipng))
  (setq-default latex-math-preview-convert-dvipng-color-mode t)
  (setq-default latex-math-preview-select-preview-window nil))

(add-hook 'LaTeX-mode-hook #'(lambda () (TeX-engine-set "luatex")))
(add-hook 'LaTeX-mode-hook #'auctex-latexmk-setup)
(add-hook 'LaTeX-mode-hook #'flycheck-mode-on-safe)
(add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook #'reftex-mode)
(setq-default reftex-plug-into-AUCTeX t)
(with-eval-after-load "company"
  (add-hook 'LaTeX-mode-hook #'company-auctex-init)
  (with-eval-after-load "reftex"
    (add-hook 'reftex-mode-hook
              #'(lambda ()
                  (add-to-list 'company-backends 'company-reftex-citations)
                  (add-to-list 'company-backends 'company-reftex-labels)))))

(defun build-with-latexmk ()
  (interactive)
  (TeX-save-document (TeX-master-file))
  (TeX-command "LatexMk" 'TeX-master-file -1))
(eval-after-load "evil-leader"
  (dolist (mode '(latex-mode tex-mode))
    (evil-leader/set-key-for-mode mode
      "m b" 'build-with-latexmk
      "m p" 'latex-math-preview-expression)))

(setq-default TeX-view-program-selection '((output-pdf "Zathura")))

(eval-after-load "auctex-latexmk"
  (setq-default auctex-latexmk-inherit-TeX-PDF-mode nil))

(provide '30-latex)
;;; 30-latex.el ends here
