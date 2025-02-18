;;; init-python.el -*- lexical-binding: t no-byte-compile: t -*-

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
;; Python Mode
;; Install: pip install flake8 black pytest
(use-package python
  :ensure t
  :hook ((inferior-python-mode . (lambda ()
                                   (process-query-on-exit-flag
                                    (get-process "Python"))))
         (python-mode . flycheck-mode))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  (setq python-shell-interpreter "python3"))

;; Live Coding in Python
(use-package live-py-mode
  :ensure t)

;; 运行 Python REPL
(require 'init-funcs)
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-p") 'run-python-in-vterm))


;; 代码格式化: Black (推荐) 或 autopep8 (二选一)
(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

;; (use-package py-autopep8
;;   :ensure t
;;   :hook (python-mode . py-autopep8-enable-on-save)) ;; 如果要用 autopep8，取消注释

;; 调试支持 (DAP)
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1))

;; Python 测试 (pytest)
(use-package pytest
  :ensure t
  :config
  (setq pytest-command "pytest"))

(use-package highlight-indent-guides
  :straight t
  :hook (python-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)  ;; 采用竖线风格
  (setq highlight-indent-guides-responsive 'top))   ;; 仅高亮当前层级

(add-hook 'python-mode-hook #'hs-minor-mode)
(defun my-python-hide-all ()
  "打开 Python 文件时自动折叠所有代码块。"
  (hs-minor-mode 1)  ;; 启用 hs-minor-mode
  (hs-hide-all))      ;; 折叠所有代码块
(add-hook 'python-mode-hook #'my-python-hide-all)

(setq python-indent-offset 4)  ;; Python 代码缩进 4 个空格
(electric-indent-mode 1)
(add-hook 'python-mode-hook 'display-line-numbers-mode) ;开启行号



(provide 'init-python)
