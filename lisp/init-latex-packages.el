;; AUCTeX
(use-package tex
  :defer t
  :init
  (setq TeX-command-default "LatexMk"
        TeX-engine 'pdflatex
        TeX-auto-save t
        TeX-parse-self t)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  :config
  (require 'preview))

;; auctex-latexmk
(use-package auctex-latexmk
  :defer t
  :init (auctex-latexmk-setup)
  :config (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;; evil-tex
(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode)
  :config
  (setq evil-tex-toggle-override-m nil)
  (setq evil-tex-toggle-override-t nil))

;; company-auctex, company-math, company-reftex
(use-package company-auctex
  :defer t
  :config
  (spacemacs|add-company-backends
   :backends (company-auctex-macros company-auctex-symbols company-auctex-environments)
   :modes LaTeX-mode))

(use-package company-math
  :defer t
  :config
  (spacemacs|add-company-backends
   :backends (company-math-symbols-unicode company-math-symbols-latex)
   :modes LaTeX-mode))

(use-package company-reftex
  :defer t
  :config
  (spacemacs|add-company-backends
   :backends (company-reftex-labels company-reftex-citations)
   :modes LaTeX-mode))

;; Reftex
(use-package reftex
  :hook (LaTeX-mode . turn-on-reftex)
  :config
  (setq reftex-plug-into-AUCTeX '(nil nil t t t)
        reftex-use-fonts t))

;; Magic LaTeX Buffer
(use-package magic-latex-buffer
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'magic-latex-buffer))

;; PDF tools (optional if needed)
(use-package pdf-tools
  :if (eq latex-view-with-pdf-tools t)
  :config
  (pdf-tools-install))

;; Key bindings for LaTeX
(with-eval-after-load 'tex
  (define-key LaTeX-mode-map (kbd "C-c C-m") 'TeX-insert-macro)
  (define-key LaTeX-mode-map (kbd "C-c C-k") 'TeX-kill-job)
  (define-key LaTeX-mode-map (kbd "C-c C-v") 'TeX-view))

;; Define font bindings for LaTeX
(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c C-b") 'latex/font-bold)
  (define-key LaTeX-mode-map (kbd "C-c C-e") 'latex/font-emphasis)
  ;; Add more bindings as needed
)

;; Reftex bindings
(with-eval-after-load 'reftex
  (define-key reftex-mode-map (kbd "C-c C-r") 'reftex-reference))

(defun latex/build ()
  (interactive)
  (let ((TeX-save-query nil))
    (TeX-save-document (TeX-master-file)))
  (TeX-command "LatexMk" 'TeX-master-file -1))

(defun latex/auto-fill-mode ()
  (interactive)
  (auto-fill-mode)
  (setq auto-fill-function 'latex//autofill))

(defun latex//autofill ()
  ;; Your auto-fill logic
  )

(defun latex/font-bold () (interactive) (TeX-font nil ?\C-b))
(defun latex/font-emphasis () (interactive) (TeX-font nil ?\C-e))
;; Add more font definitions here...


(provide 'init-latex-packages)
