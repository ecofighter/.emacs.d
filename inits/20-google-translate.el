;;; 20-google-translate.el -- google transrate
;;; Commentary:
;;; Code:
(require 'mymacros)
(require '10-shackle)

(install-when-compile 'google-translate)

(setq-default google-translate-default-source-language "en")
(setq-default google-translate-default-target-language "ja")
(add-to-list 'shackle-rules '("*Google Translate*" :popup t :select t))

(with-eval-after-load 'google-translate-tk
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

(provide '20-google-translate)
;;; 20-google-translate.el ends here
