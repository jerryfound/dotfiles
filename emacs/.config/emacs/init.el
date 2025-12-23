;; 使用 use-package 管理软件包
(require 'package)
(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(require 'use-package)

(setq use-package-always-ensure t)    ; 自动安装缺少的插件

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; 设置备份文件位置
(let ((backup-dir (expand-file-name "emacs/backups" xdg-state-home)))
  (setq backup-directory-alist (list (cons "." backup-dir)))
  (make-directory backup-dir t))

;; 保留最近5次备份
(setq version-control t) ; 启用版本控制
(setq kept-new-versions 5) ; 保留最近5个版本
(setq kept-old-versions 0) ; 不保留旧版本
(setq delete-old-versions t) ; 自动删除超出的版本

(setq bookmark-default-file (expand-file-name "emacs/bookmarks" xdg-data-home))

(set-fringe-mode 10)      ; 边缘留白
(tooltip-mode -1)         ; 禁用提示框

;; 启动时最大化
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(global-visual-line-mode 1)

(use-package ef-themes
  :ensure t
  :config
  ;; 主题轮换函数
  (defun ef-custom/cycle-themes ()
    "在 4 个 ef 主题中轮换"
    (interactive)
    (pcase (car custom-enabled-themes)
      ('ef-elea-light (load-theme 'ef-spring t))
      ('ef-spring (load-theme 'ef-bio t))
      ('ef-bio (load-theme 'ef-elea-dark t))
      (_ (load-theme 'ef-elea-light t)))
    (message "Switched to: %s" (car custom-enabled-themes)))
  
  ;; 快捷键
  (global-set-key (kbd "C-c t") 'ef-custom/cycle-themes)
  
  ;; 启动时加载
  (load-theme 'ef-elea-dark t))

(set-face-attribute 'default nil :font "Maple Mono NF CN-15")

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(setq recentf-save-file (expand-file-name "emacs/recentf" xdg-state-home))
(recentf-mode 1)
(setq recentf-max-saved-items 50)

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)))

(use-package org :ensure t)

(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)))

(setq org-directory "~/jerryfound.me/org.jerryfound.me/")

(with-eval-after-load 'org
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sp" . "src python"))
  (add-to-list 'org-structure-template-alist '("se" . "src emacs-lisp")))

(defun my/org-toggle-display-mode ()
  "切换 org-mode 的编辑模式和阅读模式"
  (interactive)
  (if org-hide-emphasis-markers
      ;; 切换到编辑模式
      (progn
        (setq-local org-hide-emphasis-markers nil)
        (setq-local org-link-descriptive nil)
        (setq-local org-pretty-entities nil)
        (org-indent-mode -1)
        (message "编辑模式"))
    ;; 切换到阅读模式
    (progn
      (setq-local org-hide-emphasis-markers t)
      (setq-local org-link-descriptive t)
      (setq-local org-pretty-entities t)
      (org-indent-mode 1)
      (message "阅读模式")))
  (font-lock-flush)
  (font-lock-ensure))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c p") 'my/org-toggle-display-mode))

(defun org-custom/font ()
  (setq buffer-face-mode-face '(:family "LXGW Bright Code" :height 180 :weight light))
  (buffer-face-mode)
  (setq-local line-spacing 0.25))

(add-hook 'org-mode-hook 'org-custom/font)

(custom-set-faces
 '(org-level-1 ((t (:height 1.3 :weight bold))))
 '(org-level-2 ((t (:height 1.2 :weight bold))))
 '(org-level-3 ((t (:height 1.1 :weight bold))))
 '(org-level-4 ((t (:height 1.0 :weight bold))))
 '(org-level-5 ((t (:height 1.0))))
 '(org-level-6 ((t (:height 1.0))))
 '(org-level-7 ((t (:height 1.0))))
 '(org-level-8 ((t (:height 1.0)))))

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

;; 关闭全局的 log-done
(setq org-log-done nil)

;; 只为"完成"状态添加 CLOSED 时间戳
(defun my/org-add-closed-on-done ()
  "只有切换到'完成'状态时添加 CLOSED 时间戳，其他状态移除 CLOSED"
  (if (string= org-state "完成")
      (org-add-planning-info 'closed (org-current-effective-time))
    (org-add-planning-info nil nil 'closed)))

(add-hook 'org-after-todo-state-change-hook 'my/org-add-closed-on-done)

;; 定义删除线应用函数
(defun my/org-done-strike-through ()
  "Add strike-through to DONE headlines"
  (setq org-fontify-done-headline t)
  (set-face-attribute 'org-headline-done nil
                      :strike-through t
                      :inherit 'org-done))

;; 初始加载时应用
(with-eval-after-load 'org
  (my/org-done-strike-through))

;; 主题切换后自动重新应用
(advice-add 'load-theme :after
            (lambda (&rest _)
              (my/org-done-strike-through)))

(use-package org-journal :ensure t)

(setq org-journal-dir "~/jerryfound.me/notes.jerryfound.me/journal/")
(setq org-journal-file-type 'yearly)
(setq org-journal-file-format "%Y-journal.org")
(setq org-journal-date-format "%Y-%m-%d %A")

(add-hook 'org-journal-mode-hook
          (lambda ()
            (face-remap-set-base 'org-level-1 :inherit 'org-level-1 :height 0.85 :weight 'bold)
            (face-remap-set-base 'org-level-2 :inherit 'org-level-2 :height 0.83 :weight 'normal)
            (face-remap-set-base 'org-level-3 :inherit 'org-level-3 :height 0.91 :weight 'normal)
            (face-remap-set-base 'org-level-4 :inherit 'org-level-4 :height 1.0 :weight 'normal)
            (face-remap-set-base 'org-level-5 :inherit 'org-level-5 :height 1.0 :weight 'normal)
            (face-remap-set-base 'org-level-6 :inherit 'org-level-6 :height 1.0 :weight 'normal)
            (face-remap-set-base 'org-level-7 :inherit 'org-level-7 :height 1.0 :weight 'normal)
            (face-remap-set-base 'org-level-8 :inherit 'org-level-8 :height 1.0 :weight 'normal)))

;; 辅助函数, 定位到当天的 journal entry 末尾
(defun org-journal-find-location ()
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

;; 辅助函数, 在 journal 文件内直接插入
(defun org-journal-custom/insert-entry (type)
  "在今天的 journal 插入指定类型的 entry"
  (org-journal-find-location)
  (insert "** " type " ")
  (insert (format-time-string "%H:%M "))
  (widen))

;; 选择函数, 选择相应类型, 插入 template
(defun org-journal-custom/new-entry-with-type ()
  "选择类型并插入 entry"
  (interactive)
  (let ((type (read-char-choice 
               "条目类型: [t]待办, [e]事件, [n]笔记: " 
               '(?t ?e ?n))))
    (org-journal-custom/insert-entry 
     (cond
      ((eq type ?t) "待办")
      ((eq type ?e) "事件")
      ((eq type ?n) "笔记")))))

;; 重新绑定 C-c C-j（在 org-journal-mode 里）
(with-eval-after-load 'org-journal
  (define-key org-journal-mode-map (kbd "C-c C-j") 
    'org-journal-custom/new-entry-with-type))

(setq org-capture-templates
      '(("j" "日记")
        ("jt" "待办" plain
         (function org-journal-find-location)
         "** 待办 %<%H:%M> %?"
         :empty-lines 1)
        ("je" "事件" plain
         (function org-journal-find-location)
         "** 事件 %<%H:%M> %?"
         :empty-lines 1)
        ("jn" "笔记" plain
         (function org-journal-find-location)
         "** 笔记 %<%H:%M> %?"
         :empty-lines 1)))

(global-set-key (kbd "C-c c") 'org-capture)

(use-package org-roam
  :ensure t)

(setq org-roam-directory "~/jerryfound.me/notes.jerryfound.me")

;; 设置 org-roam.db 文件位置, 并确保目录存在
(setq org-roam-db-location 
      (expand-file-name "emacs/org-roam.db" xdg-state-home))
(make-directory (file-name-directory org-roam-db-location) t)

;; 设置 org-id-locations 文件位置, 并确保目录存在
(setq org-id-locations-file 
      (expand-file-name "emacs/org-id-locations" xdg-cache-home))
(make-directory (file-name-directory org-id-locations-file) t)

;; db 自动同步
(org-roam-db-autosync-mode)

(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n p") 'org-id-get-create)

;; Devonthink
(org-link-set-parameters "x-devonthink-item"
  :follow (lambda (path)
            (shell-command (concat "open 'x-devonthink-item:" path "'"))))

(global-set-key (kbd "C-c r") (lambda ()
				(interactive)
				(load-file "~/.config/emacs/init.el")
				(revert-buffer t t t)
				(message "init.el reload.")))
