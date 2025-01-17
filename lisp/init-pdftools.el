;; (when (memq window-system '(mac ns))
;;   ;; solves issue of not buildling in macOS
;;   (setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig:/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig"))
;; (use-package pdf-tools :init (pdf-tools-install))
;; Emacs 29 启用了新变量名
;; (if (version<= emacs-version "28.2")
;;     (setq native-comp-deferred-compilation-deny-list '(".*pdf.*")) ; 禁用 =pdf-tools= 有关文件的本地化编译
;;   (setq native-comp-jit-deferred-compilation-deny-list '(".*pdf.*")))
                                        ; 禁用 =pdf-tools= 有关文件的本地化编译


;; (setq pdf-packages '(pdf-tools
;;                      pdf-view-restore))


(use-package image-roll
  :straight (image-roll :build t
             :type git
             :host github
             :repo "dalanicolai/image-roll.el"))


(use-package pdf-tools
  :ensure t
  :defer t
  :magic ("%PDF" . pdf-view-mode)
  ;; :straight (:build t)
  :straight (pdf-tools :build t
                       :type git
                       :host github
                       :repo "dalanicolai/pdf-tools"
                       :branch "pdf-roll"
                       :files ("lisp/*.el"
                               "README"
                               ("build" "Makefile")
                               ("build" "server")
                               (:exclude "lisp/tablist.el" "lisp/tablist-filter.el")))
  :custom
  (pdf-view-use-scaling nil)    ;; Disable scaling for better performance
  (pdf-tools-handle-upgrades t) ;; Enable automatic upgrades
  :init
  (let ((m1-brew "/opt/homebrew")
        (intel-brew "/usr/local")
        (pkg-paths '("/lib/pkgconfig"
                     "/opt/zlib/lib/pkgconfig"
                     "/opt/poppler/lib/pkgconfig"
                     "/share/pkgconfig")))
    (setenv "PKG_CONFIG_PATH"
            (mapconcat
             'identity
             (append
              (mapcar (lambda (path) (concat m1-brew path)) pkg-paths)
              (mapcar (lambda (path) (concat intel-brew path)) pkg-paths))
             ":")))
  :hook
  ((pdf-tools-enabled . pdf-view-midnight-minor-mode)
   (pdf-view-mode . (lambda () (pdf-view-fit-width-to-window) (pdf-view-roll-minor-mode))))




  :config
  (pdf-tools-install)
  (require 'pdf-annot)
  (require 'pdf-history)
  (with-eval-after-load 'pdf-view
     (csetq pdf-view-midnight-colors '("#657b83" . "#fdf6e3")) ;sepia color set
    ;; (csetq pdf-view-midnight-colors '("#d8dee9" . "#2e3440")) ;dark night
    ;; (csetq pdf-view-midnight-colors '("#ffffff" . "#1e1e1e")) ;white-black
    ;; (csetq pdf-view-midnight-colors '("#000000" . "#fffff0"))
                                        ;old paper
     )
  )



;; 将 pdf-view-mode 设置为打开 PDF 文件的默认方式
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))



(use-package pdf-view-restore
  :after pdf-tools
  :defer t
  :straight (:build t)
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename (expand-file-name ".tmp/pdf-view-restore"
                                                    user-emacs-directory)))


;; Transient state setup for pdf-tools
;; (use-package transient
;;   :config
;;   (define-transient-state pdf-tools
;;                           :title "PDF-tools Transient State"
;;                           :hint-is-doc t
;;                           :bindings
;;                           ("?" (lambda () (interactive) (message "Help: Use 'j' to scroll down, 'k' to scroll up.")))
;;                           ;; Navigation
;;                           ("j" pdf-view-next-line-or-next-page)
;;                           ("k" pdf-view-previous-line-or-previous-page)
;;                           ("l" image-forward-hscroll)
;;                           ("h" image-backward-hscroll)
;;                           ("J" pdf-view-next-page)
;;                           ("K" pdf-view-previous-page)
;;                           ("u" pdf-view-scroll-down-or-previous-page)
;;                           ("d" pdf-view-scroll-up-or-next-page)
;;                           ("0" image-bol)
;;                           ("$" image-eol)
;;                           ;; Scale/Fit
;;                           ("W" pdf-view-fit-width-to-window)
;;                           ("H" pdf-view-fit-height-to-window)
;;                           ("P" pdf-view-fit-page-to-window)
;;                           ("m" pdf-view-set-slice-using-mouse)
;;                           ("b" pdf-view-set-slice-from-bounding-box)
;;                           ("R" pdf-view-reset-slice)
;;                           ("zr" pdf-view-scale-reset)
;;                           ;; Annotations
;;                           ("aD" pdf-annot-delete)
;;                           ("at" pdf-annot-attachment-dired :exit t)
;;                           ("al" pdf-annot-list-annotations :exit t)
;;                           ("am" pdf-annot-add-markup-annotation)
;;                           ;; Actions
;;                           ("s" pdf-occur :exit t)
;;                           ("O" pdf-outline :exit t)
;;                           ("p" pdf-misc-print-document :exit t)
;;                           ("o" pdf-links-action-perform :exit t)
;;                           ("r" pdf-view-revert-buffer)
;;                           ("t" pdf-annot-attachment-dired :exit t)
;;                           ("n" pdf-view-midnight-minor-mode)
;;                           ;; Other
;;                           ("q" nil :exit t)))

  ;; Optional: set colors for midnight mode
  ;(with-eval-after-load 'pdf-view
    ;(setq pdf-view-midnight-colors '("#d8dee9" . "#2e3440"))))






(provide 'init-pdftools)
