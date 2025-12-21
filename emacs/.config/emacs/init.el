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

;; 启动时最大化
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; 使用 use-package 管理软件包
(require 'package)
(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(require 'use-package)

(setq use-package-always-ensure t)    ; 自动安装缺少的插件

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin :no-confirm))

(set-face-attribute 'default nil :font "Maple Mono NF CN-15")

(use-package org
   :ensure t
   :custom
   (org-confirm-babel-evaluate nil)
   (org-src-fontify-natively t)
   (org-src-tab-acts-natively t)
   (org-edit-src-content-indentation 0)

   :config
   (defun org-custom/org-src-indentation ()
     (when (eq major-mode 'emacs-lisp-mode)
       (setq-local org-edit-src-content-indentation 2))))

(setq org-directory "~/jerryfound.me/org.jerryfound.me/")

(setq org-todo-keywords
      '((sequence "待办(t)" "|" "完成(d)" "迁移(m)" "排期(s)" "取消(c)")
	(type "事件(e)" "|")
	(type "笔记(n)" "|")))

(setq org-todo-keyword-faces
    '(("待办" . (:foreground "#f38ba8" :weight bold))   ; 柔和红
      ("完成" . (:foreground "#a6e3a1" :weight bold))   ; 柔和绿
      ("迁移" . (:foreground "#fab387" :weight bold))   ; 柔和橙
      ("排期" . (:foreground "#f9e2af" :weight bold))   ; 柔和黄
      ("取消" . (:foreground "#6c7086" :weight bold))   ; 灰色
      ("事件" . (:foreground "#89b4fa" :weight bold))   ; 柔和蓝
      ("笔记" . (:foreground "#cba6f7" :weight bold)))) ; 柔和紫

(setq org-default-notes-file (concat org-directory "/journal.org"))
 
(setq org-capture-tempaltes
      '(("t" "任务" entry (file+datetree "")
	 "* 待办 %(format-time-string \"%H:%M\") %?")
	("e" "事件" entry (file+datetree "")
	 "* 事件 %(format-time-string \"%H:%M\") %?")
	("n" "笔记" entry (file+datetree "")
	 "* 笔记 %(format-time-string \"%H:%M\") %?")))

(global-set-key (kbd "C-c c") 'org-capture)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(recentf-mode 1)
(setq recentf-max-saved-items 50)
(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)))
