;; NOTES
;; WINDOWS
;; To make cygwin work under windows (:() add C:/cygwin/bin to the path.  Easy
;; way to do this on locked down windows machines is to use regedit.  The
;; directory is HKEY_CURRENT_USER/ENVIRONMENT - add a PATH variable of type
;; REG_EXPAND_SZ
;; LINUX
;; On debian, do this to get firefox working
;; sudo update-alternatives --install /usr/bin/gnome-www-browser gnome-www-browser /home/.../firefox-bin 20
;; and then run update-alternatives --config gnome-www-browser

;; Hide the tool bar and the scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Packages
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(defconst very-important-packages
  '(
    auto-complete
    flycheck
    org
    zenburn-theme
    ))

(defconst quite-important-packages
  '(
    ansible
    auto-complete
    dired+
    epl
    flycheck
    flycheck-color-mode-line
    flycheck-pyflakes
    fuzzy-match
    grep+
    jedi
    json-mode
    magit
    markdown-mode
    markdown-mode+
    nginx-mode
    virtualenvwrapper
    web-mode
    yaml-mode
    ))

;; Confirm kill...too easy to hit C-x C-c by accident
(setq confirm-kill-emacs 'yes-or-no-p)

(require 'cl-lib)
(defun cal-ensure-packages-installed (packages)
  (interactive)
  (when (cl-notevery 'package-installed-p packages)
    (package-refresh-contents)
    (dolist (pkg packages)
      (when (not (package-installed-p pkg))
        (package-install pkg)))))
(cal-ensure-packages-installed very-important-packages)

;; Never split horizontally, vertically only if there's lots of space
(setq split-height-threshold 200)
(setq split-width-threshold nil)

;; Backup configuration
(setq version-control t ;; Use version numbers for backups.
      kept-new-versions 10 ;; Number of newest versions to keep.
      kept-old-versions 2 ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t ;; Copy all files, don't rename them.
      backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")) ;; set backup directory
      )

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook 'force-backup-of-buffer)

;; Fix copy and paste
(if (string-equal system-type "gnu/linux")
    (progn
      (setq x-select-enable-clipboard t)
      (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)))

;; Theme
(load-theme 'zenburn t)

;; Hippie expand
(global-set-key "\M-/" 'hippie-expand)

;; Turn off the buzzer -_-
(setq ring-bell-function 'ignore)

;; Middle click paste at point
(setq mouse-yank-at-point t)

;; Windmove
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

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

;; Assume postgres
(add-hook 'sql-mode-hook
          (lambda ()
            (sql-set-product 'postgres)))

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

;; Magit
(global-set-key (kbd "C-x v w") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

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

;; Line numbering
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (progn (require 'linum)
         (global-linum-mode)))

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

;; icicles
;; (require 'icicles)
;; (icicle-mode)

;; helm
;; (require 'helm-config)
;; (helm-mode 1)
;; (define-key global-map [remap find-file] 'helm-find-files)
;; (define-key global-map [remap occur] 'helm-occur)
;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
;; (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (unless (boundp 'completion-in-region-function)
;;   (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
;;   (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
;; (projectile-global-mode)
;; (setq projectile-completion-system 'helm)
;; (helm-projectile-on)

;; Fix colours in compilation window
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Keybindings for playing with windowing
(global-set-key (kbd "C-x \"") 'ibuffer)

;; Unset markdown-mode's M-left and M-right
(add-hook 'markdown-mode-hook
          (lambda ()
            (local-unset-key (kbd "<M-left>"))
            (local-unset-key (kbd "<M-right>"))
            (local-unset-key (kbd "<M-down>"))
            (local-unset-key (kbd "<M-up>"))))

; Python
(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)))

(when (fboundp 'electric-indent-mode)
  (electric-indent-mode -1))

; Org
(setq org-directory "~/Dropbox/documents/")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(require 'uniquify)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-listing-switches "-alh")
 '(global-flycheck-mode t nil (flycheck))
 '(helm-mode-fuzzy-match t)
 '(py-underscore-word-syntax-p nil)
 '(shell-file-name "/bin/sh")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
