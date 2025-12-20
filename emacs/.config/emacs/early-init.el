;; 定义 XDG 目录变量
(defvar xdg-data-home (or (getenv "XDG_DATA_HOME")
      			  (expand-file-name "~/.local/share")))
(defvar xdg-state-home (or (getenv "XDG_STATE_HOME")
      			   (expand-file-name "~/.local/state")))

;; 设置 elpa 目录位置
(setq package-user-dir (expand-file-name "emacs/elpa" xdg-data-home))
(make-directory package-user-dir t)

;; 设置 auto-save 目录位置
(setq auto-save-list-file-prefix
      (expand-file-name "emacs/auto-save-list/.saves-" xdg-state-home))
(make-directory (file-name-directory auto-save-list-file-prefix) t)

(setq inhibit-startup-message t)  ; 禁用启动画面
(menu-bar-mode -1)        ; 禁用菜单栏
(tool-bar-mode -1)        ; 禁用工具栏
(scroll-bar-mode -1)      ; 禁用滚动条

(setq visible-bell t)     ; 可视化 Bell
