(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :straight (:host github :repo "vedang/pdf-tools" :pin "30b50544e55b8dbf683c2d932d5c33ac73323a16")
  :magic ("%PDF" . pdf-view-mode)
  :init
  (with-eval-after-load 'pdf-annot
    (defun my/pdf-cleanup-windows-h ()
      "Kill left-over annotation buffers when the document is killed."
      (when (buffer-live-p pdf-annot-list-document-buffer)
        (pdf-info-close pdf-annot-list-document-buffer))
      (when (buffer-live-p pdf-annot-list-buffer)
        (kill-buffer pdf-annot-list-buffer))
      (let ((contents-buffer (get-buffer "*Contents*")))
        (when (and contents-buffer (buffer-live-p contents-buffer))
          (kill-buffer contents-buffer))))
    (add-hook 'pdf-view-mode-hook
              (lambda () (add-hook 'kill-buffer-hook #'my/pdf-cleanup-windows-h nil t))))

  :config
  (defun my/pdf--install-epdfinfo-a (fn &rest args)
    "Install epdfinfo after the first PDF file, if needed."
    (if (and (require 'pdf-info nil t)
             (or (pdf-info-running-p)
                 (ignore-errors (pdf-info-check-epdfinfo) t)))
        (apply fn args)
      (fundamental-mode)
      (message "Viewing PDFs in Emacs requires epdfinfo. Use `M-x pdf-tools-install` to build it")))
  (advice-add #'pdf-view-mode :around #'my/pdf--install-epdfinfo-a)

  (pdf-tools-install-noverify)

  (define-key pdf-view-mode-map (kbd "q") #'kill-current-buffer)

  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)

 (setq display-buffer-alist
      '(("^\\*Outline*" (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right) (window-width . 40) (reusable-frames . visible))
        ("^\\*Edit Annotation " (display-buffer-reuse-window display-buffer-pop-up-window))
        ("\\(?:^\\*Contents\\|'s annots\\*$\\)" nil)))


  (add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode)

  (add-hook 'pdf-view-mode-hook
          (lambda ()
            (setq evil-normal-state-cursor (list nil))))

  (defun my/pdf-reload-midnight-minor-mode-h ()
    (when pdf-view-midnight-minor-mode
      (pdf-info-setoptions
       :render/foreground (car pdf-view-midnight-colors)
       :render/background (cdr pdf-view-midnight-colors)
       :render/usecolors t)
      (pdf-cache-clear-images)
      (pdf-view-redisplay t)))
  (put 'pdf-view-midnight-colors 'custom-set
       (lambda (sym value)
         (set-default sym value)
         (dolist (buffer (doom-buffers-in-mode 'pdf-view-mode))
           (with-current-buffer buffer
             (if (get-buffer-window buffer)
                 (my/pdf-reload-midnight-minor-mode-h)
               (add-hook 'doom-switch-buffer-hook #'my/pdf-reload-midnight-minor-mode-h
                         nil 'local))))))

  (defun my/pdf-suppress-large-file-prompts-a (fn size op-type filename &optional offer-raw)
    (unless (string-match-p "\\.pdf\\'" filename)
      (funcall fn size op-type filename offer-raw)))
  (advice-add #'abort-if-file-too-large :around #'my/pdf-suppress-large-file-prompts-a))


;; Install and configure saveplace-pdf-view
(use-package saveplace-pdf-view
  ;; :straight (:host github :repo "alphapapa/saveplace-pdf-view" :pin "70e9ec40565021f4b5d51e4523f4c716183a8eef")
  :after pdf-view)

(provide 'my-pdftools)
