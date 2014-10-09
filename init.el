;; Hide the tool bar and the scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Packages
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defconst important-packages
  '(
    ansible
    company
    crontab-mode
    csv-mode
    dired+
    epl
    flycheck
    flycheck-color-mode-line
    flycheck-pyflakes
    flycheck-tip
    flymake-rust
    fuzzy-match
    git-commit-mode
    git-rebase-mode
    gitignore-mode
    grep+
    icicles
    jedi
    json-mode
    magit
    markdown-mode
    markdown-mode+
    nginx-mode
    pkg-info
    popup
    puppet-mode
    ruby-block
    ruby-end
    rust-mode
    virtualenvwrapper
    web-mode
    yaml-mode
    zenburn-theme
    ))

(require 'cl-lib)
(when (cl-notevery 'package-installed-p important-packages)
  (package-refresh-contents)
  (dolist (pkg important-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Theme
(load-theme 'zenburn t)

;; Hippie expand
(global-set-key "\M-/" 'hippie-expand)

;; Revert all buffers
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Reverted open files."))

;; Flycheck on by default
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook #'flycheck-color-mode-line-mode)

;; Get rid of the splash screen
(setq inhibit-splash-screen t)

;; Set a sensible fill column
(setq-default fill-column 79)

;; Compile
(global-set-key [f5] 'compile)

;; Fill region is a common command
(global-set-key (kbd "M-#") 'fill-region)

;; Magic to fix an ubuntu keyboard layout
(global-set-key [dead-grave] "`")

;; M-up and M-down do the same as C-up and C-down
(global-set-key [(meta up)] 'backward-paragraph)
(global-set-key [(meta down)] 'forward-paragraph)

;; Magit
(global-set-key (kbd "C-x v w") 'magit-status)

;; TRAMP root magic (commented because it interferes with normal sudo use
;; (set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;; Nuclear whitespace mode
(setq-default indent-tabs-mode nil)
(setq-default delete-trailing-lines nil)
(add-hook 'write-file-hooks
          (lambda ()
            (if (not indent-tabs-mode)
                (save-excursion (untabify (point-min) (point-max))
                                (delete-trailing-whitespace)))))

;; Use chromium instead of ff
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

;; Line numbering
(require 'linum)
(global-linum-mode)

;; Full Screen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

;; (require 'auto-complete)
;; (global-auto-complete-mode t)
(add-hook 'after-init-hook 'global-company-mode)

;; Always do syntax highlighting
(global-font-lock-mode 1)

;; Match parenthesis
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; Try to fix the scrolling behaviour
(setq scroll-conservatively 1) ; scroll line by line with no jumps

;; Turn on column numbers in the mode line
(column-number-mode t)

(require 'icicles)
(icicle-mode)

;; Fix colours in compilation window
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Crontab
(add-to-list 'load-path "~/.emacs.d/crontab/")
(require 'crontab-mode)

;; Keybindings for playing with windowing
(global-set-key [f11] 'toggle-fullscreen)
(global-set-key (kbd "C-x \"") 'ibuffer)

;; Unset markdown-mode's M-left and M-right
(add-hook 'markdown-mode-hook
          (lambda ()
            (local-unset-key (kbd "<M-left>"))
            (local-unset-key (kbd "<M-right>"))
            (local-unset-key (kbd "<M-down>"))
            (local-unset-key (kbd "<M-up>"))))

(add-hook 'python-mode-hook
          (lambda ()
            (company-mode -1)
            (jedi:setup)))

(require 'uniquify)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-flycheck-mode t nil (flycheck))
 '(shell-file-name "/bin/sh")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 108 :width normal :foundry "xos4" :family "Terminus"))))
 '(flycheck-color-mode-line-error-face ((t (:inherit flycheck-fringe-error :foreground "#ff0000" :weight normal))))
 '(flycheck-color-mode-line-warning-face ((t (:inherit flycheck-fringe-warning :foreground "#ffda00" :weight normal)))))
(put 'downcase-region 'disabled nil)
