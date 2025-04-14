(use-package eaf
  :load-path "~/.vanilla/site-lisp/emacs-application-framework"
  :custom
  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (defalias 'browse-web #'eaf-open-browser)
  ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  ;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  ) ;; unbind, see more in the Wiki
(add-to-list 'load-path "~/.vanilla/site-lisp/emacs-application-framework/")

(require 'eaf)
(require 'eaf-pdf-viewer)

(setq eaf-pdf-dark-mode t)
;; (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
;; (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
;; (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))









(provide 'init-eafpdfviewer)
