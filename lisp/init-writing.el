;;; init-writing.el -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2021-2023 zilongshanren

;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/emacs.d


;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;






(use-package ispell-minor-mode
  :ensure nil
  :config
  (advice-add 'ispell-lookup-words :around
              (lambda (orig &rest args)
                (shut-up (apply orig args)))))

(use-package flyspell-correct
  :ensure t
  :init

  )

(use-package ispell
  :ensure nil
  :init
  (if sys/win32p
      (setq ispell-program-name "aspell"))
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (ispell-change-dictionary "american" t)
  (setq text-mode-ispell-word-completion nil))

(use-package corfu-english-helper
  :ensure nil
  :commands toggle-corfu-english-helper)



(use-package ox-hugo
  :ensure t                             ;Auto-install the package from Melpa
  :pin melpa ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox
  :commands org-hugo-export-to-md)




(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


(provide 'init-writing)
