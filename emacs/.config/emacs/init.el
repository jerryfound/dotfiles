;; 设置备份文件位置
(let ((backup-dir (expand-file-name "emacs/backups" xdg-state-home)))
  (setq backup-directory-alist (list (cons "." backup-dir)))
  (make-directory backup-dir t))

;; 保留最近5次备份
(setq version-control t) ; 启用版本控制
(setq kept-new-versions 5) ; 保留最近5个版本
(setq kept-old-versions 0) ; 不保留旧版本
(setq delete-old-versions t) ; 自动删除超出的版本

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(set-fringe-mode 10)      ; 边缘留白
(tooltip-mode -1)         ; 禁用提示框

(require 'package)
(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(require 'use-package)

(setq use-package-always-ensure t)    ; 自动安装缺少的插件

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)

  :config
  (load-theme 'doom-nord t)

  (doom-themes-org-config))

(set-face-attribute 'default nil :font "Maple Mono NF CN-15")

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)))
