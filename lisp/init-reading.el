;;; init-reading.el -*- lexical-binding: t no-byte-compile: t -*-

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

;; (use-package olivetti
;;   :init
;;   (setq olivetti-body-width nil)
;;   :config
;;   (defun distraction-free ()
;;     "Distraction-free writing environment"
;;     (interactive)
;;     (if (equal olivetti-mode nil)
;;         (olivetti-mode t)
;;       (progn
;;         (olivetti-mode 0))))
;;   :bind
;;   (("<f9>" . distraction-free)))


(use-package org-noter
  :ensure t
  :init
  (setq-default org-noter-always-create-frame nil)
  )



(use-package nov
  :ensure t
  :config
  :defer t
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))


;; (use-package dictionary-overlay
;;   :demand t
;;   :ensure nil)

(provide 'init-reading)
