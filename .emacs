;; Hide the tool bar and the scroll bar
(tool-bar-mode -1) 
(scroll-bar-mode -1)

;; Packages
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-refresh-contents)

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
 '(
   auto-complete
   crontab-mode
   dired+
   egg
   icicles
   markdown-mode
   markdown-mode+
   nginx-mode
   puppet-mode
   web-mode
   yaml-mode
   zenburn-theme
   ))

;; Theme
(load-theme 'zenburn t)

;; Revert all buffers
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
	(revert-buffer t t t) )))
  (message "Reverted open files."))

;; Get rid of the splash screen
(setq inhibit-splash-screen t)

;; Set a sensible fill column
(setq-default fill-column 79)

;; Compile
(global-set-key [f5] 'compile)

;; Nuclear whitespace mode
(add-hook 'write-file-hooks (lambda () (if (not indent-tabs-mode)
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

(require 'auto-complete)
(global-auto-complete-mode t)

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

(require 'egg)
(egg-minor-mode)

;; Crontab
(add-to-list 'load-path "~/.emacs.d/crontab/")
(require 'crontab-mode)

;; Keybindings for playing with windowing
(global-set-key [f11] 'toggle-fullscreen)
(global-set-key (kbd "C-x \"") 'list-buffers)

(require 'uniquify)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
