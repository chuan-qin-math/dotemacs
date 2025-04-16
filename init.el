;;; init.el -*- lexical-binding: t no-byte-compile: t -*-

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
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with \ f f and enter text in its buffer.
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;


;; part of the code are from Centaur emacs
;;; Code:
;; (require 'cl)

(setq package-check-signature nil)

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

(when (not (version< emacs-version "29.0"))
  (setq package-native-compile nil))

;; Speed up startup
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; If `file-name-handler-alist' is nil, no 256 colors in TUI
    ;; @see https://emacs-china.org/t/spacemacs-centaur-emacs/3802/839
    (setq file-name-handler-alist
          (unless (display-graphic-p)
            '(("\\(?:\\.tzst\\|\\.zst\\|\\.dz\\|\\.txz\\|\\.xz\\|\\.lzma\\|\\.lz\\|\\.g?z\\|\\.\\(?:tgz\\|svgz\\|sifz\\)\\|\\.tbz2?\\|\\.bz2\\|\\.Z\\)\\(?:~\\|\\.~[-[:alnum:]:#@^._]+\\(?:~[[:digit:]]+\\)?~\\)?\\'" . jka-compr-handler))))
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 80000000
                  gc-cons-percentage 0.1)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp" "elpa"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(require 'init-package)

(use-package benchmark-init
  :ensure t
  :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; the follows path setup is only for macOS (M chips), please add your path for dependent tools (required by pdftools, ripgrep, etc...)!
;; 设置 Homebrew 的 bin 路径
(let ((homebrew-bin "/opt/homebrew/bin"))
  ;; 添加到 exec-path
  (add-to-list 'exec-path homebrew-bin)
  ;; 更新 PATH 环境变量
  (setenv "PATH" (concat homebrew-bin ":" (getenv "PATH"))))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(add-to-list 'exec-path "/usr/local/bin")


(setq my-is-terminal (not window-system))


;; Core
(require 'init-basic)
(require 'init-completion)

(require 'init-evil)

(require 'init-tools)
(require 'init-writing)
(require 'init-reading)

;; uis
(require 'init-ui)
(require 'init-window)
(require 'init-better-defaults)

;; Tools
(require 'init-org)
(require 'init-git)
(require 'init-ctags)
(require 'init-syntaxcheck)
(require 'init-snippets)

(require 'init-lsp)

;; Frameworks
;; yaml init , shell etc
(require 'init-persp)


;; Languages
(require 'init-lisp)
(require 'init-python)

 ;; (require 'init-pdftools)
;; (require 'init-eafpdfviewer)
;; (require 'init-latex-packages)
(require 'init-latex-config)
;; (require 'my-pdftools)
;; (require 'init-latex-config2)
;; personal
(require 'init-keybindings)


;; programming
(require 'init-programming)

;; weather
(require 'init-weather)

(advice-add 'quail-input-method :around
            (lambda (orig key)
              "Disable quail-input-method in TeX and LaTeX formulas. Uses AUCTeX."
              (if (and (eq major-mode 'latex-mode)
                       (fboundp 'texmathp)
                       (texmathp))
                  (let ((overriding-local-map t))
                    (funcall orig key))
                (funcall orig key))))


(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
