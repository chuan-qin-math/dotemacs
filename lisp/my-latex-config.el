;; AUCTeX configuration
(use-package auctex
  :defer t
  :straight (:build t)
  :hook ((LaTeX-mode . LaTeX-preview-setup) ;; 保证 AUCTeX 在 LaTeX-mode 下启动
         (LaTeX-mode . turn-on-reftex)      ;; 启用 reftex
         (LaTeX-mode . TeX-source-correlate-mode)
         (TeX-mode . visual-line-mode) ;; 启用 visual-line-mode
         (TeX-mode . lsp-deferred)     ;; 启用 LSP 支持
         (LaTeX-mode . lsp-deferred))
  :init
  ;; 设置 AUCTeX 相关变量
  (setq TeX-command-default (if (executable-find "latexmk") "LatexMk" "LaTeX")
        TeX-engine (if (executable-find "xetex") 'xetex 'default)
        TeX-auto-save t
        TeX-parse-self t
        TeX-syntactic-comment t
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        TeX-source-correlate-method 'synctex
        TeX-electric-sub-and-superscript t
        TeX-fill-break-at-separators nil
        TeX-save-query t
        TeX-master t)
  :config
  (setq font-latex-match-reference-keywords
        '(;; BibLaTeX.
          ("printbibliography" "[{") ("addbibresource" "[{")
          ;; Standard commands.
          ("cite" "[{")       ("citep" "[{")
          ("citet" "[{")      ("Cite" "[{")
          ("parencite" "[{")  ("Parencite" "[{")
          ("footcite" "[{")   ("footcitetext" "[{")
          ;; Style-specific commands.
          ("textcite" "[{")   ("Textcite" "[{")
          ("smartcite" "[{")  ("Smartcite" "[{")
          ("cite*" "[{")      ("parencite*" "[{")
          ("supercite" "[{")
          ;; Qualified citation lists.
          ("cites" "[{")      ("Cites" "[{")
          ("parencites" "[{") ("Parencites" "[{")
          ("footcites" "[{")  ("footcitetexts" "[{")
          ("smartcites" "[{") ("Smartcites" "[{")
          ("textcites" "[{")  ("Textcites" "[{")
          ("supercites" "[{")
          ;; Style-independent commands.
          ("autocite" "[{")   ("Autocite" "[{")
          ("autocite*" "[{")  ("Autocite*" "[{")
          ("autocites" "[{")  ("Autocites" "[{")
          ;; Text commands.
          ("citeauthor" "[{") ("Citeauthor" "[{")
          ("citetitle" "[{")  ("citetitle*" "[{")
          ("citeyear" "[{")   ("citedate" "[{")
          ("citeurl" "[{")
          ;; Special commands.
          ("fullcite" "[{")
          ;; Cleveref.
          ("cref" "{")          ("Cref" "{")
          ("cpageref" "{")      ("Cpageref" "{")
          ("cpagerefrange" "{") ("Cpagerefrange" "{")
          ("crefrange" "{")     ("Crefrange" "{")
          ("labelcref" "{")))

  (setq font-latex-match-textual-keywords
        '(;; BibLaTeX brackets.
          ("parentext" "{") ("brackettext" "{")
          ("hybridblockquote" "[{")
          ;; Auxiliary commands.
          ("textelp" "{")   ("textelp*" "{")
          ("textins" "{")   ("textins*" "{")
          ;; Subcaption.
          ("subcaption" "[{")))

  (setq font-latex-match-variable-keywords
        '(;; Amsmath.
          ("numberwithin" "{")
          ;; Enumitem.
          ("setlist" "[{")     ("setlist*" "[{")
          ("newlist" "{")      ("renewlist" "{")
          ("setlistdepth" "{") ("restartlist" "{")
          ("crefname" "{")))
  ;; (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")
  (add-hook 'TeX-mode-hook (lambda ()
                             (setq ispell-parser          'tex
                                   fill-nobreak-predicate (cons #'texmathp fill-nobreak-predicate))))
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  ;; Keybindings
  (general-define-key
   :keymaps '(latex-mode-map LaTeX-mode-map)
   :package 'auctex ;; 添加 :package 参数
   "l" '(:keymap lsp-command-map :which-key "lsp")
   "v" '(TeX-view :which-key "View")
   "c" '(TeX-command-run-all :which-key "Compile")
   "m" '(TeX-command-master :which-key "Run a command"))

  ;; Hook for LSP
  (add-hook 'tex-mode-hook 'lsp-deferred)
  (add-hook 'latex-mode-hook 'lsp-deferred)




  )



(setq font-latex-match-reference-keywords
      '(;; BibLaTeX.
        ("printbibliography" "[{") ("addbibresource" "[{")
        ;; Standard commands.
        ("cite" "[{")       ("citep" "[{")
        ("citet" "[{")      ("Cite" "[{")
        ("parencite" "[{")  ("Parencite" "[{")
        ("footcite" "[{")   ("footcitetext" "[{")
        ;; Style-specific commands.
        ("textcite" "[{")   ("Textcite" "[{")
        ("smartcite" "[{")  ("Smartcite" "[{")
        ("cite*" "[{")      ("parencite*" "[{")
        ("supercite" "[{")
        ;; Qualified citation lists.
        ("cites" "[{")      ("Cites" "[{")
        ("parencites" "[{") ("Parencites" "[{")
        ("footcites" "[{")  ("footcitetexts" "[{")
        ("smartcites" "[{") ("Smartcites" "[{")
        ("textcites" "[{")  ("Textcites" "[{")
        ("supercites" "[{")
        ;; Style-independent commands.
        ("autocite" "[{")   ("Autocite" "[{")
        ("autocite*" "[{")  ("Autocite*" "[{")
        ("autocites" "[{")  ("Autocites" "[{")
        ;; Text commands.
        ("citeauthor" "[{") ("Citeauthor" "[{")
        ("citetitle" "[{")  ("citetitle*" "[{")
        ("citeyear" "[{")   ("citedate" "[{")
        ("citeurl" "[{")
        ;; Special commands.
        ("fullcite" "[{")
        ;; Cleveref.
        ("cref" "{")          ("Cref" "{")
        ("cpageref" "{")      ("Cpageref" "{")
        ("cpagerefrange" "{") ("Cpagerefrange" "{")
        ("crefrange" "{")     ("Crefrange" "{")
        ("labelcref" "{")))

(setq font-latex-match-textual-keywords
      '(;; BibLaTeX brackets.
        ("parentext" "{") ("brackettext" "{")
        ("hybridblockquote" "[{")
        ;; Auxiliary commands.
        ("textelp" "{")   ("textelp*" "{")
        ("textins" "{")   ("textins*" "{")
        ;; Subcaption.
        ("subcaption" "[{")))

(setq font-latex-match-variable-keywords
      '(;; Amsmath.
        ("numberwithin" "{")
        ;; Enumitem.
        ("setlist" "[{")     ("setlist*" "[{")
        ("newlist" "{")      ("renewlist" "{")
        ("setlistdepth" "{") ("restartlist" "{")
        ("crefname" "{")))

;; AUCTeX configuration
(use-package tex-mode
  :defer t
  :straight (:type built-in)
  :config
  (setq LaTeX-section-hook '(LaTeX-section-heading
                             LaTeX-section-title
                             LaTeX-section-toc
                             LaTeX-section-section
                             LaTeX-section-label)
        LaTeX-fill-break-at-separators nil
        LaTeX-item-indent 0))

;; Preview configuration for LaTeX documents
(use-package preview
  :defer t
  :straight (:type built-in)
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))
  (setq preview-auto-cache-preamble nil)
  (phundrak/major-leader-key
    :packages 'auctex
    :keymaps '(latex-mode-map LaTeX-mode-map)
    "p" #'preview-at-point
    "P" #'preview-clearout-at-point))




(use-package cdlatex
  :straight (:host github :repo "cdominik/cdlatex" )
  :defer 10
  :after tex ; cdlatex 需要 auctex 中的 texmathp.el
  :config
  ;; 导入 cdlatex 自定义设置
  (setq cdlatex-paired-parens "$[{(")
  ;; (my/set-cdlatex-math-symbol-alist)
  ;; (my/set-cdlatex-math-modify-alist)
  ;; (my/set-cdlatex-env-alist)
  ;; (my/set-cdlatex-command-alist)
  )



(use-package tex
  :defer 10
  :straight auctex
  :custom
  (TeX-parse-self t) ; 自动解析 tex 文件found
  (TeX-PDF-mode t)
  (TeX-DVI-via-PDFTeX t)
  ;; (TeX-PDF-from-DVI "Dvips") ; for preview latex, see https://www.reddit.com/r/emacs/comments/bsoko7/comment/eor96mj/
  ;; 正向与反向搜索设置
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex)
  (TeX-view-program-selection '((output-pdf "PDF Tools"))) ; 使用 pdf-tools 预览 pdf
  (TeX-source-correlate-start-server t)
  :config
  ;; 设置 LaTeX 语法高亮颜色及字体大小
  ;; (setq-default TeX-master t)
                                        ; 默认询问主文件
  (add-hook 'LaTeX-mode-hook 'my/latex-hook) ; 加载LaTeX模式设置
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer) ; 编译后更新 pdf 文件
  ;; (my/TeX-fold-config)
  ;; (my/TeX-fonts-config)
  ;; (my/preview-latex-config)
  ;; (my/reftex-config)
  ;; (my/more-prettified-symbols)
  )

(use-package auctex-latexmk
  :straight (auctex-latexmk :fetcher github :repo "emacsmirror/auctex-latexmk") ; fix a incompatibility bug by non-author.
  :defer 5
  :after tex
  :init
  (auctex-latexmk-setup)
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (add-to-list 'TeX-command-list
               '("myLatexMK" "latexmk %(-PDF)%S%(mode) %(file-line-error) -pvc %(extraopts) %t" TeX-run-latexmk nil (plain-tex-mode latex-mode doctex-mode) :help "Run LatexMK with -pvc")))

;; Adaptive Wrap configuration for LaTeX mode
(use-package adaptive-wrap
  :defer t
  :after auctex
  :straight (:build t)
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 0))



;; Company AUCTeX for LaTeX auto-completion
(use-package company-auctex
  :defer t
  :after (company auctex)
  :straight (:build t)
  :config
  (company-auctex-init))

;; Company Math for math symbol auto-completion
(use-package company-math
  :defer t
  :after (company auctex)
  :straight (:build t)
  :config
  (defun my-latex-mode-setup ()
    (setq-local company-backends
                (append '((company-math-symbols-latex company-latex-commands))
                        company-backends)))
  (add-hook 'TeX-mode-hook #'my-latex-mode-setup))



(provide 'my-latex-config)
