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
(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode))

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



(provide 'init-python)
