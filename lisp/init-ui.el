;;; init-ui.el -*- lexical-binding: t no-byte-compile: t -*-

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

(setq inhibit-splash-screen t)
(setq-default cursor-type 'bar)

(setq  initial-frame-alist (quote ((fullscreen . maximized))))

(global-hl-line-mode t)

;; (global-display-line-numbers-mode t)
(setq frame-title-format
      `((buffer-file-name "%f" "%b")
        ,(format " - GNU Emacs %s" emacs-version)))

;; copied from Phundrak
(setq package-enable-at-startup nil
      inhibit-startup-message   t
      frame-resize-pixelwise    t       ; fine resize
      package-native-compile    t) ; native compile packages
(scroll-bar-mode -1)               ; disable scrollbar
(tool-bar-mode -1)                 ; disable toolbar
(tooltip-mode -1)                  ; disable tooltips
(set-fringe-mode 10)               ; give some breathing room
(menu-bar-mode -1)                 ; disable menubar
(blink-cursor-mode 0)              ; disable blinking cursor
;; never want to keep trailing spaces in my files
(add-hook 'before-save-hook #'whitespace-cleanup)
;; Don't add two spaces behind a full stop
(setq-default sentence-end-double-space nil)
;;better jump words
(global-subword-mode 1)
;; cursor stay on top or bottom while the text scrolls progressively
;; (setq scroll-conservatively 1000)
;; Indentation
(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))
;; folding code
;; (dolist (mode '(<<prog-modes-gen()>>))
;;  (add-hook mode #'hs-minor-mode))


(setq delete-by-moving-to-trash t)



(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-solarized-light t))

(set-face-attribute 'default nil :height 200)

(use-package valign
  :ensure t
  :hook ((markdown-mode org-mode) . valign-mode))


(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-minor-modes t)
  :custom-face
  (mode-line ((t (:height 0.95))))
  (mode-line-inactive ((t (:height 0.95))))
  :hook (after-init . doom-modeline-mode))

;; copied from Phundrak
;It is expected of files to be encoded with LF UTF-8, so only show
;the encoding in the modeline if the encoding is worth notifying
;the user."
(defun modeline-contitional-buffer-encoding ()
 (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))


;; Now, let’s automate the call to this function in order to apply the
;; modifications to the modeline each time we open a new file.

(add-hook 'after-change-major-mode-hook #'modeline-contitional-buffer-encoding)

;; set font: Cascadia code
(defvar phundrak/default-font-size 90
  "Default font size.")

(defvar phundrak/default-font-name "Cascadia Code"
  "Default font.")

(defun my/set-font ()
  (when (find-font (font-spec :name phundrak/default-font-name))
    (set-face-attribute 'default nil
                        :font phundrak/default-font-name
                        :height phundrak/default-font-size)))

(my/set-font)
(add-hook 'server-after-make-frame-hook #'my/set-font)


(use-package visual-fill-column
  :ensure t
  :init
  ;; Configure fill width
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t))

(provide 'init-ui)
