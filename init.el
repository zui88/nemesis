;;; This fixed garbage collection, makes emacs start up faster ;;;;;;;
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
	gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is all kinds of necessary
(require 'package)
(setq package-enable-at-startup nil)

;;; remove SC if you are not using sunrise commander and org if you like outdated packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("gnu-elpa" . "https://elpa.gnu.org/packages/")
))
(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make sure that use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;; This is the actual config file. It is omitted if it doesn't exist so emacs won't refuse to launch.
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))


;; opacity
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
;; (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 50)))
(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 85) '(100 . 100)))))
;;         '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)


;; quick opening init-file
(defun open-file-init ()
  (interactive)
  (find-file-existing "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e") 'open-file-init)


;; obvious stuff
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq ring-bell-function nil)
(setq locale-coding-system 'utf-8)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)


;; backup directory
;; (defun my-backup-file-name (fpath)
;;   "Return a new file path of a given file path.
;; If the new path's directories does not exist, create them."
;;   (let* ((backupRootDir "~/.emacs.d/backup/")
;;         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
;;         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
;;     (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
;;     backupFilePath))
;; (setq make-backup-file-name-function 'my-backup-file-name)


(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )


;; cursor position
(setq line-number-mode t)
(setq column-number-mode t)
(when window-system (add-hook 'prog-mode-hook 'hl-line-mode))
(setq scroll-preserve-screen-position t)
(global-prettify-symbols-mode 1)


;; better scrolling 
(setq scroll-conservatively 100)


;; buffer stuff
(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.


It returns the buffer (for elisp programing)."
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)$buf))
(global-set-key (kbd "<f7>") 'xah-new-empty-buffer)
(setq initial-major-mode (quote org-mode))


(global-set-key (kbd "C-x b") 'ibuffer)
;;expert mode --noconfirmation
(setq ibuffer-expert t)


;; switch buffers
(use-package helm
  :ensure t
  :after (async popup)
  :diminish (helm-mode)
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x r b" . helm-filtered-bookmarks)
  ("C-x C-b" . helm-buffers-list)
  :config
  (helm-mode 1))

;; (setq ido-enable-flex-matching t)
;; (setq ido-create-new-buffer 'always)
;; (setq ido-everywhere t)
;;(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(ido-mode 1)


;;(global-set-key (kbd "C-j") 'deadreth/return)

(defun deadreth/copy-whole-word ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (backward-word)
    (kill-word 1)
    (yank)))
(global-set-key (kbd "C-c w c") 'deadreth/copy-whole-word)

(defun deadreth/kill-inner-word ()
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1)
  (just-one-space))
(global-set-key (kbd "C-c w k") 'deadreth/kill-inner-word)

(defun deadreth/copy-whole-line ()
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))
(global-set-key (kbd "C-c l c") 'deadreth/copy-whole-line)

(global-set-key (kbd "C-c l k") 'kill-whole-line)

(defun kill-curr-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-curr-buffer)

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-M-s-k") 'kill-all-buffers)


(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


;; "C-x M-g"
(use-package magit
  :ensure t
  :config
  (setq magit-clone-always-transient t))
(global-set-key (kbd "C-x g") 'magit)


(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 8)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
	'("a" "s" "d" "f" "g" "h" "j" "k" "l"))
  :bind
  ([remap other-window] . switch-window))

(use-package ace-window
  :ensure t
  :config
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t))
(global-set-key (kbd "M-o") 'ace-window)

;; Rotate the positions of the window
(use-package rotate
  :ensure t)

;; cursor follows split
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode 1))

(use-package linum-relative
  :ensure t
  :diminish linum-relative-mode
  :config
  (setq linum-relative-current-symbol "")
  (add-hook 'prog-mode-hook 'linum-relative-mode))

;; (use-package spaceline
;;   :ensure t
;;   :config
;;   (require 'spaceline-config)
;;   (setq powerline-default-seperator (quote arrow))
;;   (spaceline-emacs-theme))

(use-package async
  :ensure t
  :config
  (dired-async-mode 1))
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; zipping
(eval-after-load "dired-aux"
   '(add-to-list 'dired-compress-file-suffixes 
                 '("\\.zip\\'" ".zip" "unzip")))


(use-package popup
  :ensure t)


;; (use-package ido-vertical-mode
;;   :ensure t
;;   :config
;;   (ido-vertical-mode 1))
;; (setq ido-vertical-define-keys 'C-n-and-C-p-only)


(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-Iosvkem t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
    ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode))

(use-package avy
	     :ensure t
	     :bind
	     ("M-s s" . avy-goto-char)
	     ("M-s f" . avy-goto-char-2)	     
	     ("M-s g" . avy-goto-line)
	     ("M-s SPC" . avy-goto-char-timer)
	     ("M-s c" . avy-copy-line))

(defun copy-whole-line ()
    (interactive)
    (save-excursion
      (kill-new
       (buffer-substring
	(point-at-bol)
	(point-at-eol)))))
(global-set-key (kbd "C-c w l") 'copy-whole-line)

(use-package ivy-rich
  :ensure t
  :after (counsel)
  :config
  (ivy-rich-mode 1))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . 'swiper))

(setq electric-pair-pairs '((?\{ . ?\})
			    (?\( . ?\))
			    (?\[ . ?\])
			    (?\" . ?\")))
(electric-pair-mode t)
(setq electric-pair-inhibit-predicate
       (lambda (c)
	 (if (char-equal c ?\<) t (electric-pair-default-inhibit c))))

;;;;;;;;;;;;;;;;;;;;
;;org stuff


(defun my/copy-id-to-clipboard()
  "Copy the ID property value to killring,
if no ID is there then create a new unique ID. 
This function works only in org-mode buffers.

The purpose of this function is to easily construct id:-links to 
org-mode items. If its assigned to a key it saves you marking the
text and copying to the killring."
       (interactive)
       (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
	 (setq mytmpid (funcall 'org-id-get-create))
	 (kill-new mytmpid)
	 (message "Copied %s to killring (clipboard)" mytmpid)))
(global-set-key (kbd "<f5>") 'my/copy-id-to-clipboard)

;; camel case
;; maybe in some modes ... set hooks
;(global-subword-mode)


;; super user - sudo
(use-package sudo-edit
  :ensure t
  :bind
  ("C-c C-u" . sudo-edit))


(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package org-pdftools
  :ensure t
  :after
  (pdf-tools)
  :hook
  (org-mode . org-pdftools-setup-link))

(add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "s x") 'pdf-view-auto-slice-minor-mode)))
;;(add-hook 'pdf-view-mode-hook (lambda () (pdf-view-auto-slice-minor-mode 1)))
(add-hook 'pdf-view-mode-hook (lambda () (pdf-view-midnight-minor-mode 1)))
(add-hook 'pdf-view-mode-hook (lambda () (pdf-view-fit-page-to-window)))


(use-package diminish
  :ensure t
  :config
  (diminish 'page-break-lines-mode)
  (diminish 'eldoc-mode)
  (diminish 'auto-fill-mode)
  (diminish 'subword-mode)
  (diminish 'auto-revert-mode)
  (diminish 'pdf-view-midnight-minor-mode))

;; for more convenient tabbing
(global-set-key (kbd "C-x t h") 'tab-previous)
(global-set-key (kbd "C-x t l") 'tab-next)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-subword-mode nil)
 '(package-selected-packages
   '(spice-mode counsel-projectile lsp-treemacs helm-lsp lsp-ui lsp-mode company-jedi eglot switch-window vterm org-roam yasnippet-snippets which-key use-package symon swiper sudo-edit spaceline rotate rainbow-delimiters projectile org-superstar org-pdftools org-journal magit linum-relative ivy-rich ido-vertical-mode hungry-delete helm flycheck-clang-analyzer expand-region doom-themes dmenu diminish dashboard company-irony company-c-headers beacon ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
