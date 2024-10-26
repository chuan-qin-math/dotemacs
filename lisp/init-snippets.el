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
  :straight (:build t)                ;; 使用 straight.el
  :hook ((prog-mode . yas-minor-mode) ;; 在编程模式下启用 yasnippet
         (latex-mode . yas-minor-mode)
         (org-mode . yas-minor-mode)   ;; 在 org-mode 下启用
         (text-mode . yas-minor-mode)) ;; 在 text-mode 下启用
  :init
  (yas-global-mode) ;; 全局启用 yasnippet
  :config
  ;; add in latex-mode the path to the snippet folder
  (add-to-list 'yas-snippet-dirs "~/.vanilla/snippets/LaTeX-mode")

  ;; 手动重新加载代码片段
  (yas-reload-all)


  ;; (defun yas-hippie-try-expand (first-time)
  ;;   "Integrate `yasnippet` into `hippie-expand`."
  ;;   (if (not first-time)
  ;;       (yas-expand)))

  ;; (setq hippie-expand-try-functions-list
  ;;       '(yas-hippie-try-expand ;; 先尝试展开 yasnippet 片段
  ;;         try-complete-file-name-partially
  ;;         try-expand-all-abbrevs
  ;;         try-expand-dabbrev
  ;;         try-expand-dabbrev-all-buffers
  ;;         try-expand-dabbrev-from-kill
  ;;         try-complete-lisp-symbol-partially
  ;;         try-complete-lisp-symbol))

  ;; 绑定 `hippie-expand` 到 TAB 键
  ;; (global-set-key (kbd "TAB") 'hippie-expand)

  ;; 让 hippie-expand 尝试使用 company、cape 和 corfu 的补全
 (defun my/hippie-expand-with-company ()
  "Use `company` completion with `hippie-expand`, prioritizing yasnippet and cdlatex."
  (interactive)
  (let ((company-candidates (company--candidates))
        (yas-fallback-behavior 'return-nil)) ;; 允许 yasnippet 返回 nil
    ;; 尝试展开 yasnippet
    (if (yas-expand)
        nil
      ;; 尝试使用 company 的候选项
      (if company-candidates
          (let ((completion (completing-read "Complete: " company-candidates nil t)))
            (insert completion))
        ;; 否则使用 hippie-expand
        (hippie-expand nil)))))

  ;; 将自定义的 hippie-expand 函数绑定到 TAB 键
   (global-set-key (kbd "TAB") 'my/hippie-expand-with-company)

  ;; 设置 yasnippet 展开的快捷键为 C-c y
   ;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
   ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
   (global-set-key (kbd "C-c y") 'yas-expand)

  ;; 设置插入 yasnippet 片段的快捷键为 C-c i
  (global-set-key (kbd "C-c i") 'yas-insert-snippet)

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
