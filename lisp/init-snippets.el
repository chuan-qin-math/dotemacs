;;; init-snippets.el -*- lexical-binding: t no-byte-compile: t -*-

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


;; (use-package yasnippet
;;   :ensure t
;;   :hook ((prog-mode . yas-minor-mode)
;;          (org-mode . yas-minor-mode))
;;   :init
;;   :config
;;   (progn
;;     (setq hippie-expand-try-functions-list
;;           '(yas/hippie-try-expand
;;             try-complete-file-name-partially
;;             try-expand-all-abbrevs
;;             try-expand-dabbrev
;;             try-expand-dabbrev-all-buffers
;;             try-expand-dabbrev-from-kill
;;             try-complete-lisp-symbol-partially
;;             try-complete-lisp-symbol))

;;     ;; (defun yas-setup-capf ()
;;     ;;   (setq-local completion-at-point-functions
;;     ;;               (cons #'cape-yasnippet
;;     ;;                     completion-at-point-functions)))

;;     ;; (add-hook 'prog-mode-hook 'yas-setup-capf)
;;     ;; (add-hook 'org-mode-hook 'yas-setup-capf)
;;     )

;;   )

;; (use-package consult-yasnippet
;;   :ensure t)

;; (use-package yasnippet-snippets
;;   :ensure t
;;   :after yasnippet)
(use-package yasnippet
  :ensure t
  :straight (:build t)  ;; 使用 straight.el
  :hook ((prog-mode . yas-minor-mode) ;; 在编程模式下启用 yasnippet
          (latex-mode . yas-minor-mode)
         (org-mode . yas-minor-mode)   ;; 在 org-mode 下启用
         (text-mode . yas-minor-mode)) ;; 在 text-mode 下启用
  :init
  (yas-global-mode)  ;; 全局启用 yasnippet
  :config
  (setq hippie-expand-try-functions-list
        '(yas/hippie-try-expand
          try-complete-file-name-partially
          try-expand-all-abbrevs
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))

  ;; 加载 yasnippet-snippets
  (use-package yasnippet-snippets
    :straight (:build t)
    :after yasnippet)

  ;; 加载 yatemplate，用于自动生成模板
  (use-package yatemplate
    :straight (:build t)
    :after yasnippet)

  ;; 可选：配置 `cape-yasnippet` 用于补全（注释掉的部分）
  ;; (defun yas-setup-capf ()
  ;;   (setq-local completion-at-point-functions
  ;;               (cons #'cape-yasnippet
  ;;                     completion-at-point-functions)))
  ;; (add-hook 'prog-mode-hook 'yas-setup-capf)
  ;; (add-hook 'org-mode-hook 'yas-setup-capf))

  ;; Consult-yasnippet，用于更好的 snippet 查找
  (use-package consult-yasnippet
    :ensure t))


(provide 'init-snippets)
