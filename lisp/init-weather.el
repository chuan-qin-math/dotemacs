(use-package wttrin
  :defer t
  :straight (wttrin :build t
                    :local-repo "/Users/qc/.emacs.legacy2/emacs-wttrin/"
                    :type git)
  :config
  (setq wttrin-default-cities '("Paris" "Hangzhou")  ; 设定城市为巴黎和杭州
        wttrin-use-metric t)
  (setq wttrin-default-accept-language '("Accept-Language" . "zh-CN"))  ; 使用简体中文
)


(provide 'init-weather)
