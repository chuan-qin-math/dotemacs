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
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((nov-mode . visual-line-mode)
         (nov-mode . visual-fill-column-mode)
         (nov-mode . my-nov-setup))
  :config
  (setq nov-text-width 100) ;; 根据需要调整宽度
  (defun my-nov-setup ()
    "Setup visual-fill-column to center content in nov-mode."
    (setq visual-fill-column-center-text t)
    (setq visual-fill-column-width 80))
  ;; FIXME: errors while opening epub files with Unicode characters
  ;; See https://emacs-china.org/t/emacs-epub/4713/5 关于EPUB extraction failed with exit code 9 Mark, 乱码, EPUB 3 ID not found等问题的讨论,以下代码抄自该讨论中回复
  (with-no-warnings
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (when-let* ((name (nov-content-unique-identifier-name content))
                  (selector (format "package>metadata>identifier[id='%s']"
                                    (regexp-quote name)))
                  (id (car (esxml-node-children (esxml-query selector content)))))
        (intern id)))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier))

  (defun nov--content-epub2-files (content manifest files)
    (let* ((node (esxml-query "package>spine[toc]" content))
           (id (esxml-node-attribute 'toc node)))
      (when (not id)
        (throw 'error "EPUB 2 NCX ID not found"))
      (setq nov-toc-id (intern id))
      (let ((toc-file (assq nov-toc-id manifest)))
        (when (not toc-file)
          (throw 'error "EPUB 2 NCX file not found"))
        (cons toc-file files))))

  (defun nov--content-epub3-files (content manifest files)
    (let* ((node (esxml-query "package>manifest>item[properties~=nav]" content))
           (id (esxml-node-attribute 'id node)))
      (when (not id)
        (throw 'error "EPUB 3 <nav> ID not found"))
      (setq nov-toc-id (intern id))
      (let ((toc-file (assq nov-toc-id manifest)))
        (when (not toc-file)
          (throw 'error "EPUB 3 <nav> file not found"))
        (setq files (--remove (eq (car it) nov-toc-id) files))
        (cons toc-file files))))

  (defun nov-content-files (directory content)
    "Create correctly ordered file alist for CONTENT in DIRECTORY.
Each alist item consists of the identifier and full path."
    (let* ((manifest (nov-content-manifest directory content))
           (spine (nov-content-spine content))
           (files (mapcar (lambda (item) (assq item manifest)) spine)))
      (catch 'error (nov--content-epub3-files content manifest files))
      (catch 'error (nov--content-epub2-files content manifest files))))
  (with-eval-after-load 'shr
    (set-face-attribute 'variable-pitch nil :font (format "%s:pixelsize=%d" "PingFang SC" 24)))

  )


;; (use-package dictionary-overlay
;;   :demand t
;;   :ensure nil)

(provide 'init-reading)
