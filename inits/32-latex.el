;;; 32-latex.el -- write latex; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'mymacros)
(require '31-lsp)
(install-when-compile 'lsp-latex)
;; (install-when-compile 'auctex)
;; (install-when-compile 'auctex-latexmk)
;; (install-when-compile 'company-auctex)
;; (install-when-compile 'company-reftex)
;; (install-when-compile 'company-math)
;; (install-when-compile 'latex-math-preview)

;; (with-eval-after-load "latex-math-preview"
;;   (setq-default latex-math-preview-command-path-alist
;;                 '((platex . "uplatex")
;;                   (pdflatex . "lualatex")
;;                   (dvipng . "dvipng")
;;                   (dvips . "dvips")
;;                   (gs . "gs")))
;;   (setq-default latex-math-preview-tex-to-png-for-preview '(platex dvipng))
;;   (setq-default latex-math-preview-tex-to-png-for-save '(platex dvipng))
;;   (setq-default latex-math-preview-tex-to-eps-for-save '(platex dvips-to-eps))
;;   (setq-default latex-math-preview-tex-to-ps-for-save '(platex dvips-to-ps))
;;   (setq-default latex-math-preview-beamer-to-png '(platex dvipng))
;;   (setq-default latex-math-preview-convert-dvipng-color-mode t)
;;   (setq-default latex-math-preview-select-preview-window nil))

;; (add-hook 'LaTeX-mode-hook #'(lambda ()
;;                                (TeX-engine-set "luatex")
;;                                (auctex-latexmk-setup)
;;                                (flycheck-mode-on-safe)
;;                                (TeX-source-correlate-mode)
;;                                (LaTeX-math-mode)
;;                                (reftex-mode)
;;                                (setq-default TeX-auto-save t)
;;                                (setq-default TeX-parse-self t)))
;; (with-eval-after-load "company"
;;   (add-hook 'LaTeX-mode-hook #'company-auctex-init)
;;   (with-eval-after-load "reftex"
;;     (setq-default reftex-plug-into-AUCTeX t)
;;     (add-hook 'reftex-mode-hook
;;               #'(lambda ()
;;                   (add-to-list 'company-backends 'company-reftex-citations)
;;                   (add-to-list 'company-backends 'company-reftex-labels)))))

;; (defun build-with-latexmk ()
;; "Build buffer with latexmk."
;;   (interactive)
;;   (TeX-save-document (TeX-master-file))
;;   (TeX-command "LatexMk" 'TeX-master-file -1))

;; (with-eval-after-load "evil-leader"
;;   (dolist (mode '(latex-mode tex-mode))
;;     (evil-leader/set-key-for-mode mode
;;       "m b" 'build-with-latexmk
;;       "m p" 'latex-math-preview-expression)))

;; (setq-default TeX-view-program-selection '((output-pdf "Zathura")))

;; (with-eval-after-load "auctex-latexmk"
;;   (setq-default auctex-latexmk-inherit-TeX-PDF-mode nil))
(with-eval-after-load "tex-mode"
  (require 'lsp-latex)
  (setq-default lsp-latex-build-args '("-lualatex" "-interaction=nonstopmode" "-synctex=1" "%f"))
  (setq-default lsp-latex-forward-search-executable "zathura")
  (setq-default lsp-latex-forward-search-args '("--synctex-forward" "%l:1:%f" "%p"))
  (add-hook 'tex-mode-hook 'lsp)
  (add-hook 'latex-mode-hook 'lsp))

(with-eval-after-load "bibtex"
  (require 'lsp-latex)
  (setq-default lsp-latex-build-args '("-lualatex" "-interaction=nonstopmode" "-synctex=1" "%f"))
  (add-hook 'bibtex-mode-hook 'lsp))

(provide '32-latex)
;;; 32-latex.el ends here
