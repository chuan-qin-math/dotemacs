;; (when (memq window-system '(mac ns))
;;   ;; solves issue of not buildling in macOS
;;   (setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig:/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig"))
;; (use-package pdf-tools :init (pdf-tools-install))
;; Emacs 29 启用了新变量名
(if (version<= emacs-version "28.2")
    (setq native-comp-deferred-compilation-deny-list '(".*pdf.*")) ; 禁用 =pdf-tools= 有关文件的本地化编译
  (setq native-comp-jit-deferred-compilation-deny-list '(".*pdf.*"))) ; 禁用 =pdf-tools= 有关文件的本地化编译


(setq pdf-packages '(pdf-tools
                     pdf-view-restore))

(defun pdf/init-pdf-tools ()
  (use-package pdf-tools
    :defer t
    :magic ("%PDF" . pdf-view-mode)
    :straight (:build t)
    :custom
    (pdf-view-use-scaling nil) ;; Disable scaling for better performance
    :init
    (pdf-loader-install) ;; Install the pdf loader
    :hook (pdf-tools-enabled . pdf-view-midnight-minor-mode)
    :general
    (phundrak/evil
     :keymaps 'pdf-view-mode-map
     :packages 'pdf-tools
     "y"   #'pdf-view-kill-ring-save
     "t"   #'evil-collection-pdf-view-next-line-or-next-page
     "s"   #'evil-collection-pdf-view-previous-line-or-previous-page)
    (phundrak/major-leader-key
      :keymaps 'pdf-view-mode-map
      :packages 'pdf-tools
      "a"  '(:ignore t :which-key "annotations")
      "aD" #'pdf-annot-delete
      "at" #'pdf-annot-attachment-dired
      "ah" #'pdf-annot-add-highlight-markup-annotation
      "al" #'pdf-annot-list-annotations
      "am" #'pdf-annot-markup-annotation
      "ao" #'pdf-annot-add-strikeout-markup-annotation
      "as" #'pdf-annot-add-squiggly-markup-annotation
      "at" #'pdf-annot-add-text-annotation
      "au" #'pdf-annot-add-underline-markup-annotation

      "f"  '(:ignore t :which-key "fit")
      "fw" #'pdf-view-fit-width-to-window
      "fh" #'pdf-view-fit-height-to-window
      "fp" #'pdf-view-fit-page-to-window

      "s"  '(:ignore t :which-key "slice/search")
      "sb" #'pdf-view-set-slice-from-bounding-box
      "sm" #'pdf-view-set-slice-using-mouse
      "sr" #'pdf-view-reset-slice
      "ss" #'pdf-occur

      "o"  'pdf-outline
      "m"  'pdf-view-midnight-minor-mode)
    :config
    (with-eval-after-load 'pdf-view
      (csetq pdf-view-midnight-colors '("#d8dee9" . "#2e3440")))

    ;; Additional keybindings for pdf-view-mode
    (define-prefix-command 'pdf-view-prefix)
    (define-key pdf-view-mode-map (kbd "C-c p") 'pdf-view-prefix)
    (define-key pdf-view-mode-map (kbd "C-c pa") 'pdf-annot-add-highlight-markup-annotation)
    (define-key pdf-view-mode-map (kbd "C-c pd") 'pdf-annot-delete)
    (define-key pdf-view-mode-map (kbd "C-c pf") 'pdf-view-fit-width-to-window)
    (define-key pdf-view-mode-map (kbd "C-c pp") 'pdf-misc-print-document)
    (define-key pdf-view-mode-map (kbd "C-c po") 'pdf-outline)

    ;; Evilified state bindings for pdf-view-mode
    (evil-define-key 'visual pdf-view-mode-map
      "y" 'pdf-view-kill-ring-save
      (kbd "<C-down-mouse-1>") 'pdf-view-mouse-extend-region
      (kbd "<M-down-mouse-1>") 'pdf-view-mouse-set-region-rectangle
      (kbd "<down-mouse-1>")  'pdf-view-mouse-set-region)

    (evilified-state-evilify-map pdf-view-mode-map
                                 :mode  pdf-view-mode
                                 :bindings
                                 "j"  'pdf-view-next-line-or-next-page
                                 "k"  'pdf-view-previous-line-or-previous-page
                                 "J"  'pdf-view-next-page
                                 "K"  'pdf-view-previous-page
                                 "gg"  'pdf-view-first-page
                                 "G"  'pdf-view-last-page
                                 "/" 'isearch-forward
                                 "?" 'isearch-backward
                                 "r"   'pdf-view-revert-buffer
                                 "o"   'pdf-links-action-perform
                                 "O"   'pdf-outline))

  (evil-define-key 'normal pdf-outline-buffer-mode-map
    "j" 'next-line
    "k" 'previous-line
    "RET" 'pdf-outline-follow-link)

(require 'pdf-annot) ; 设置 pdf-annot-mimor-mode-map 必须
  (require 'pdf-history) ; 设置 pdf-history-minor-mode-map 必须
  (add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window) ; 默认适应页宽

  )

(defun pdf/init-pdf-view-restore ()
  (use-package pdf-view-restore
    :after pdf-tools
    :defer t
    :straight (:build t)
    :hook (pdf-view-mode . pdf-view-restore-mode)
    :config
    (setq pdf-view-restore-filename (expand-file-name ".tmp/pdf-view-restore"
                                                      user-emacs-directory))))

;; Transient state setup for pdf-tools
  (use-package transient
    :config
    (define-transient-state pdf-tools
      :title "PDF-tools Transient State"
      :hint-is-doc t
      :bindings
      ("?" (lambda () (interactive) (message "Help: Use 'j' to scroll down, 'k' to scroll up.")))
      ;; Navigation
      ("j" pdf-view-next-line-or-next-page)
      ("k" pdf-view-previous-line-or-previous-page)
      ("l" image-forward-hscroll)
      ("h" image-backward-hscroll)
      ("J" pdf-view-next-page)
      ("K" pdf-view-previous-page)
      ("u" pdf-view-scroll-down-or-previous-page)
      ("d" pdf-view-scroll-up-or-next-page)
      ("0" image-bol)
      ("$" image-eol)
      ;; Scale/Fit
      ("W" pdf-view-fit-width-to-window)
      ("H" pdf-view-fit-height-to-window)
      ("P" pdf-view-fit-page-to-window)
      ("m" pdf-view-set-slice-using-mouse)
      ("b" pdf-view-set-slice-from-bounding-box)
      ("R" pdf-view-reset-slice)
      ("zr" pdf-view-scale-reset)
      ;; Annotations
      ("aD" pdf-annot-delete)
      ("at" pdf-annot-attachment-dired :exit t)
      ("al" pdf-annot-list-annotations :exit t)
      ("am" pdf-annot-add-markup-annotation)
      ;; Actions
      ("s" pdf-occur :exit t)
      ("O" pdf-outline :exit t)
      ("p" pdf-misc-print-document :exit t)
      ("o" pdf-links-action-perform :exit t)
      ("r" pdf-view-revert-buffer)
      ("t" pdf-annot-attachment-dired :exit t)
      ("n" pdf-view-midnight-minor-mode)
      ;; Other
      ("q" nil :exit t)))

  ;; Optional: set colors for midnight mode
  ;(with-eval-after-load 'pdf-view
    ;(setq pdf-view-midnight-colors '("#d8dee9" . "#2e3440"))))






(provide 'init-pdftools)
