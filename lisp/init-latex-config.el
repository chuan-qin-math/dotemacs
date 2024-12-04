;; -*- lexical-binding: t; -*-
;; Emacs 29 启用了新变量名
(if (version<= emacs-version "28.2")
    (setq native-comp-deferred-compilation-deny-list '(".*pdf.*")) ; 禁用 =pdf-tools= 有关文件的本地化编译
  (setq native-comp-jit-deferred-compilation-deny-list '(".*pdf.*"))) ; 禁用 =pdf-tools= 有关文件的本地化编译



(defun my/set-cdlatex-command-alist ()
  (setq cdlatex-command-alist
        '(("eq" "insert pairs of \\[ \\]" "\\[ ? \\]" cdlatex-position-cursor nil t t)
          ("Big(" "insert Big ()" "\\Big( ? \\Big" cdlatex-position-cursor nil nil t)
          ("Big[" "insert Big[" "\\Big[ ? \\Big" cdlatex-position-cursor nil nil t)
          ("Big\\|" "insert Big \\|" "\\Big\\| ? \\Big\\|" cdlatex-position-cursor nil nil t)
          ("Big{" "insert Big{}" "\\Big\\{ ? \\Big\\" cdlatex-position-cursor nil nil t)
          ("Big|" "insert Big|" "\\Big| ? \\Big|" cdlatex-position-cursor nil nil t)
          ("aali" "insert equation" "\\left\\{\\begin{aligned}\n? \n\\end{aligned}\\right." cdlatex-position-cursor nil nil t)
          ("alb" "Insert beamer alert block with overlay" "\\begin{alertblock}<+->{ ? } \n\n\\end{alertblock}" cdlatex-position-cursor nil t nil)
          ("alb*" "Insert beamer alert block without overlay" "\\begin{alertblock}{ ? } \n\n\\end{alertblock}" cdlatex-position-cursor nil t nil)
          ("big(" "insert big ()" "\\big( ? \\big" cdlatex-position-cursor nil nil t)
          ("big[" "insert big []" "\\big[ ? \\big" cdlatex-position-cursor nil nil t)
          ("big\\|" "insert big \\|" "\\big\\| ? \\big\\|" cdlatex-position-cursor nil nil t)
          ("bigg(" "insert bigg()" "\\bigg( ? \\bigg" cdlatex-position-cursor nil nil t)
          ("bigg[" "insert bigg[" "\\bigg[ ? \\bigg" cdlatex-position-cursor nil nil t)
          ("bigg\\|" "insert bigg\\|" "\\bigg\\| ? \\bigg\\|" cdlatex-position-cursor nil nil t)
          ("bigg{" "insert bigg{}" "\\bigg\\{ ? \\bigg\\" cdlatex-position-cursor nil nil t)
          ("bigg|" "insert bigg|" "\\bigg| ? \\bigg|" cdlatex-position-cursor nil nil t)
          ("big{" "insert big {}" "\\big\\{ ? \\big\\" cdlatex-position-cursor nil nil t)
          ("big|" "insert big|" "\\big| ? \\big|" cdlatex-position-cursor nil nil t)
          ("blo" "Insert beamer block with overlay" "\\begin{block}<+->{ ? } \n\n\\end{block}" cdlatex-position-cursor nil t nil)
          ("blo*" "Insert beamer block WITHOUT overlay" "\\begin{block}{ ? } \n\n\\end{block}" cdlatex-position-cursor nil t nil)
          ("bn" "binomial" "\\binom{?}{}" cdlatex-position-cursor nil nil t)
          ("capl" "insert \\bigcap\\limits_{}^{}" "\\bigcap\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
          ("case" "insert cases" "\\begin{cases}\n? & \\\\\n &\n\\end{cases}" cdlatex-position-cursor nil nil t)
          ("cd" "insert cdots" "\\cdots" nil nil t t)
          ("cupl" "insert \\bigcup\\limits_{}^{}" "\\bigcup\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
          ("dd" "insert ddots" "\\ddots" nil nil t t)
          ("defn" "insert definition env" "" cdlatex-environment ("definition") t nil)
          ("des" "insert description" "" cdlatex-environment ("description") t nil)
          ("enu*" "insert enu" "\\begin{enumerate}\n\\item ?\n\\end{enumerate}" cdlatex-position-cursor nil t nil)
          ("equation*" "insert unlabel equation" "" cdlatex-environment ("equation*") t nil)
          ("exb" "Insert beamer example block with overlay" "\\begin{exampleblock}<+->{ ? } \n\n\\end{exampleblock}" cdlatex-position-cursor nil t nil)
          ("exb*" "Insert beamer example block without overlay" "\\begin{exampleblock}{ ? } \n\n\\end{exampleblock}" cdlatex-position-cursor nil t nil)
          ("exe" "Insert exercise" "\\begin{exercise}\n? \n\\end{exercise}" cdlatex-position-cursor nil t nil)
          ("fra" "insert frame (for beamer)" "" cdlatex-environment ("frame") t nil)
          ("hhl" "insert \\ \\hline" "\\\\ \\hline" ignore nil t nil)
          ("hl" "insert \\hline" "\\hline" ignore nil t nil)
          ("ipenu" "insert in paragraph enumerate" "" cdlatex-environment ("inparaenum") t nil)
          ("ipite" "insert in paragraph itemize" "" cdlatex-environment ("inparaitem") t nil)
          ("it" "insert \\item" "\\item?" cdlatex-position-cursor nil t nil)
          ("ld" "insert ldots" "\\ldots" nil nil t t)
          ("lem" "insert lemma env" "" cdlatex-environment ("lemma") t nil)
          ("liml" "insert \\lim\\limits_{}" "\\lim\\limits_{?}" cdlatex-position-cursor nil nil t)
          ("lr<" "insert bra-ket" "\\langle ? \\rangle" cdlatex-position-cursor nil nil t)
          ("myenu" "insert in my enumerate for beamer" "" cdlatex-environment ("myenumerate") t nil)
          ("myite" "insert in my itemize for beamer" "" cdlatex-environment ("myitemize") t nil)
          ("ons" "" "\\onslide<?>{ }" cdlatex-position-cursor nil t t)
          ("pa" "insert pause" "\\pause" ignore nil t nil)
          ("pro" "insert proof env" "" cdlatex-environment ("proof") t nil)
          ("prodl" "insert \\prod\\limits_{}^{}" " \\prod\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
          ("prop" "insert proposition" "" cdlatex-environment ("proposition") t nil)
          ("se" "insert \\{\\}" "\\{ ? \\}" cdlatex-position-cursor nil nil t)
          ("spl" "insert split" "" cdlatex-environment ("split") nil t)
          ("st" "stackrel" "\\stackrel{?}{}" cdlatex-position-cursor nil nil t)
          ("te" "insert text" "\\text{?}" cdlatex-position-cursor nil nil t)
          ("thm" "insert theorem env" "" cdlatex-environment ("theorem") t nil)
          ("vd" "insert vdots" "\\vdots" nil nil t t))))

(defun my/set-cdlatex-env-alist ()
  (setq cdlatex-env-alist
        '(("definition" "\\begin{definition}\n\\label{def:?}\n\n\\end{definition}" nil)
          ("enumerate" "\\begin{enumerate}[?]\n\\item \n\\end{enumerate}" "\\item ?")
          ("equation*" "\\begin{equation*}\n? \n\\end{equation*}" nil)
          ("exercise" "\\begin{exercise}[?]\n\n\\end{exercise}" nil)
          ("frame" "\\begin{frame}{ ? }\n\n\\end{frame}" nil)
          ("inparaenum" "\\begin{inparaenum}\n\\item ? \n\\end{inparaenum}" "\\item ?")
          ("inparaitem" "\\begin{inparaitem}\n\\item ?\n\\end{inparaitem}" "\\item ?")
          ("lemma" "\\begin{lemma}\n\\label{lem:?}\n\n\\end{lemma}" nil)
          ("myenumerate" "\\begin{myenumerate}\n\\item ?\n\\end{myenumerate}" "\\item ?")
          ("myitemize" "\\begin{myitemize}\n\\item ?\n\\end{myitemize}" "\\item ?")
          ("proof" "\\begin{proof}?\n\n\\end{proof}" nil)
          ("proposition" "\\begin{proposition}\n\n\\end{proposition}" nil)
          ("theorem" "\\begin{theorem}\n\\label{thm:?}\n\n\\end{theorem}" nil))))

(defun my/set-cdlatex-math-modify-alist ()
    (setq cdlatex-math-modify-alist
          '((?t "\\mathbb" "" t nil nil))))

(defun my/set-cdlatex-math-symbol-alist ()
        (setq cdlatex-math-symbol-alist
              '((?0 ("\\varnothing" "\\emptyset"))
                (?1 ("\\ONE" "\\one"))
                (?. ("\\cdot" "\\circ"))
                (?v ("\\vee" "\\bigvee"))
                (?& ("\\wedge" "\\bigwedge"))
                (?9 ("\\cap" "\\bigcap" "\\bigoplus"))
                (?+ ("\\cup" "\\bigcup" "\\oplus"))
                (?- ("\\rightharpoonup" "\\hookrightarrow" "\\circlearrowleft"))
                (?= ("\\equiv" "\\Leftrightarrow" "\\Longleftrightarrow"))
                (?~ ("\\sim" "\\approx" "\\propto"))
                (?L ("\\Lambda" "\\limits"))
                (?* ("\\times" "\\otimes" "\\bigotimes"))
                (?e ("\\eps" "\\epsilon" "\\exp\\Big( ? \\Big)"))
                (?> ("\\mapsto" "\\longrightarrow" "\\rightrightarrows"))
                (?< ("\\preceq" "\\leftarrow" "\\longleftarrow"))
                (?| ("\\parallel" "\\mid" "\\perp"))
                (?S ("\\Sigma" "\\sum_{?}^{}"))
                (?{ ("\\subset" "\\prec" "\\subseteq"))
                (?} ("\\supset" "\\succ" "\\supseteq")))))

(use-package cdlatex
  :straight (:host github :repo "cdominik/cdlatex" )
  :defer 10
  :after tex ; cdlatex 需要 auctex 中的 texmathp.el
  :config
  ;; 导入 cdlatex 自定义设置
  (setq cdlatex-paired-parens "$[{(")
  (my/set-cdlatex-math-symbol-alist)
  (my/set-cdlatex-math-modify-alist)
  (my/set-cdlatex-env-alist)
  (my/set-cdlatex-command-alist))

(defun my/TeX-fold-config ()
  (setq TeX-fold-type-list '(env macro comment)
        TeX-fold-env-spec-list '(("[comment]" ("comment")) ("[proof]" ("proof")))
        LaTeX-fold-env-spec-list '(("frame" ("frame")))
        TeX-fold-macro-spec-list
        '(("[f]" ("footnote" "marginpar"))
          ("[c]" ("cite"))
          ("[l]" ("label"))
          ("[r]" ("ref" "pageref" "eqref"))
          ("[i]" ("index" "glossary"))
          ("[1]:||*" ("item"))
          ("..." ("dots"))
          ("(C)" ("copyright"))
          ("(R)" ("textregistered"))
          ("TM" ("texttrademark"))
          (1 ("emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt" "textbf" "textsc" "textup")))))

(defun my/TeX-fonts-config ()
  (setq LaTeX-font-list
        '((?m "\\textmc{" "}" "\\mathmc{" "}")
          (?g "\\textgt{" "}" "\\mathgt{" "}")
          (?e "\\en{" "}")
          (?c "\\cn{" "}")
          (?4 "$" "$")
          (1 "" "" "\\mathcal{" "}")
          (2 "\\textbf{" "}" "\\mathbf{" "}")
          (3 "\\textsc{" "}")
          (5 "\\emph{" "}")
          (6 "\\textsf{" "}" "\\mathsf{" "}")
          (9 "\\textit{" "}" "\\mathit{" "}")
          (12 "\\textulc{" "}")
          (13 "\\textmd{" "}")
          (14 "\\textnormal{" "}" "\\mathnormal{" "}")
          (18 "\\textrm{" "}" "\\mathrm{" "}")
          (19 "\\textsl{" "}" "\\mathbb{" "}")
          (20 "\\texttt{" "}" "\\mathtt{" "}")
          (21 "\\textup{" "}")
          (23 "\\textsw{" "}")
          (4 "" "" t))))

(defun my/preview-latex-config ()
  (setq preview-default-option-list
        '("displaymath" "floats" "graphics" "textmath" "footnotes") ; 执行预览的环境
        preview-preserve-counters t ; 保留数学公式编号
        preview-pdf-color-adjust-method 'compatible)) ; 预览图片使用Emacs主题背景色

(defun my/reftex-config ()
  (setq reftex-label-alist ; 交叉引用的自定义类型
        '((nil ?e nil "\\cref{%s}" nil nil) ; 与 cref 配合使用.
          ("theorem" ?t "thm:" nil t ("Theorem" "定理"))
          ("proposition" ?p "prop:" nil t ("Proposition" "命题"))
          ("definition" ?d "def:" nil t ("Definition" ))
          ("lemma" ?a "lem:" nil t ("Lemma" "引理")))
        reftex-insert-label-flags '("s" "sftpd")
        reftex-ref-macro-prompt nil ; ~cte<tab>~ 后不提示类型
        reftex-ref-style-default-list '("Cleveref"))) ; 默认引用风格 Used to "Default"

(defun my/more-prettified-symbols ()
  (require 'tex-mode) ; 载入 tex--prettify-symbols-alist 变量
  (mapc (lambda (pair) (delete pair tex--prettify-symbols-alist))
        '(("\\supset" . 8835)))
  (mapc (lambda (pair) (cl-pushnew pair tex--prettify-symbols-alist))
        '(("\\big(" . #x2987) ; Notation left image bracket
          ("\\big)" . #x2988)
          ("\\Big(" . #x2985) ; left white parenthesis
          ("\\Big)" . #x2986)
          ("\\bigg(" . #xFF5F) ; full width left white parenthesis
          ("\\bigg)" . #xFF60)
          ("\\big[" . #x3010) ; full width left square bracket
          ("\\big]" . #x3011)
          ("\\Big[" . #x27E6) ; math left white square bracket
          ("\\Big]" . #x27E7)
          ("\\bigg[" . #x301A) ; left white square bracket
          ("\\bigg]" . #x301B)
          ("\\{" . #xFF5B) ; full width curly bracket
          ("\\}" . #xFF5D)
          ("\\big\\{" . #xFF5B) ;
          ("\\big\\}" . #xFF5D)
          ("\\Big\\{" . #xFF5B) ; white bracket
          ("\\Big\\}" . #xFF5D)
          ("\\bigg\\{" . #xFF5B) ; white bracket
          ("\\bigg\\}" . #xFF5D)
          ("\\Z" . 8484)
          ("\\Q" . 8474)
          ("\\N" . 8469)
          ("\\R" . 8477)
          ("\\eps" . 949)
          ("\\inf" . #x22C0)
          ("\\sup". #x22C1)
          ("\\ONE" . #x1D7D9)
          ("\\mathbb{S}" . #x1D54A)
          ("\\PP" . #x2119)
          ("\\Ps" . #x1D5AF )
          ("\\Pp" . #x1D40F)
          ("\\E" . #x1D5A4)
          ("\\Ee" . #x1D404)
          ("\\EE" . #x1D53C )
          ("\\Fc" . #x2131)
          ("\\Nc" . #x1D4A9))))


;; (defun my/latex-tab-command ()
;;   "Custom Tab command for LaTeX mode that prioritizes cdlatex, company-auctex, company-math, yasnippet, and auctex."
;;   (interactive)
;;   (cond
;;    ;; 优先处理 cdlatex 的 Tab 功能
;;    ((and (eq major-mode 'latex-mode) (cdlatex-tab-command)) nil)
;;    ;; 如果 company-active-p 为真，调用 company 进行补全
;;    ((company-tooltip-visible-p) (company-complete-common))
;;    ;; 否则，调用 auctex 的补全功能
;;    ((austex-active) (TeX-complete-symbol))
;;    ;; 默认缩进
;;    (t (indent-for-tab-command))))


(use-package outline
  :init
  (setq outline-minor-mode-prefix (kbd "C-o"))
  (setq outline-minor-mode-prefix (kbd "C-'"))
  :bind
  (:map outline-minor-mode-map
        ("C-' t" . outline-hide-body)
        ("C-' e" . outline-show-entry)
        ("C-' p" . outline-previous-visible-heading)
        ("C-' n" . outline-next-visible-heading)
        ("C-' a" . outline-show-all)))

(defun my/latex-hook ()
  (turn-on-cdlatex)
  (turn-on-reftex)
  (outline-minor-mode)                  ; 大纲预览
  ;; (outline-hide-body) ; 启动时折叠文件
  ;; (when my/enable-folding
  ;;   (prettify-symbols-mode t))
  )  ; prettify 数学符号

(defun pdf-util-frame-scale-factor () 2)





(use-package tex
  :defer 10
  :straight auctex
  :custom
  ;; AUCTeX 设置
  (TeX-parse-self t)                     ; 自动解析 tex 文件
  (TeX-PDF-mode t)                       ; 启用 PDF 模式
  (TeX-DVI-via-PDFTeX t)                 ; 使用 PDFTeX 生成 DVI
  (TeX-source-correlate-mode t)          ; 启用源代码关联
  (TeX-source-correlate-method 'synctex) ; 设置源代码关联方法为 SyncTeX
  (TeX-source-correlate-start-server t)  ; 启动服务器以支持关联
  ;; (TeX-view-program-selection '((output-pdf "Skim"))) ; 设置 PDF 查看程序
  ;; (TeX-view-program-list
  ;;  '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o")))
                                        ; Skim 配置
   (TeX-view-program-selection '((output-pdf "PDF Tools")))
                                        ; 使用 pdf-tools 预览 pdf





  (setq-default TeX-master t)           ; 默认询问主文件

  :hook
  ;; 加载 LaTeX 模式设置
  (LaTeX-mode . my/latex-hook)
  ;; 编译后更新 PDF 文件
  (TeX-after-compilation-finished . TeX-revert-document-buffer)

  :config
  ;; 自定义设置
  (my/TeX-fold-config)
  (my/TeX-fonts-config)
  (my/preview-latex-config)
  (my/reftex-config)
  (my/more-prettified-symbols)
  ;; (my/set-latex-font)
  ;; (my/latex-tab-command)

  ;; 设置 AUCTeX 编译文件时询问，t 为默认当前文件为主文件
  (setq-default TeX-master nil)

  ;; 启动 Emacs 服务器
  (server-start))




;; reftex 设置
(setq reftex-default-bibliography '("reference.bib"))
(setq reftex-bibliography-commands '("cite" "citep" "citet" "citeyear")) ;; 需要的引用命令
(setq reftex-label-alist '(("article" ?a "Article" "~\\cite{" nil nil)
                           ("book"    ?b "Book"    "~\\cite{" nil nil)))
(setq reftex-show-bibliography t) ;; 显示参考文献



(use-package company-auctex
  :defer t
  :after (company auctex)
  :straight (:build t)
  :config
  (company-auctex-init))

(use-package company-math
  :defer t
  :straight (:build t)
  :after (company auctex)
  :config
  (defun my-latex-mode-setup ()
    "Set up company backends for LaTeX mode."
    ;; 设置 company-backends 的顺序，使得 math, auctex 和 cdlatex 在前
    (setq-local company-backends
                (append '((cdlatex-tab company-math-symbols-latex company-latex-commands
                           company-auctex-macros company-auctex-symbols company-auctex-environments
                           ))
                        ;; 其余 backends 保持不变，但 company-dabbrev 位于后面
                        (remove 'company-dabbrev company-backends)
                        '(company-dabbrev))))
  (add-hook 'TeX-mode-hook #'my-latex-mode-setup)
  (add-hook 'LaTeX-mode-hook #'my-latex-mode-setup))


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


(provide 'init-latex-config)
