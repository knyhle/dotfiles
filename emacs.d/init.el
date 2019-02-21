;; disable bars
(tool-bar-mode   -1)
(menu-bar-mode   -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; toggle qol variables
(column-number-mode t)
(electric-pair-mode t)
(global-display-line-numbers-mode t)

(setq frame-title-format "")
(setq show-paren-delay 0)
(setq inhibit-startup-message t)

(setq default-directory "/Users/kennyl/")
(setq x-select-enable-clipboard t)

;; change font
(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :height 100
		    :weight 'normal)

;; start package initialization
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"   ) t)
;(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/") t)
(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package paren
  :config
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "#def"))

(use-package atom-one-dark-theme :ensure t
  :config
  (load-theme 'atom-one-dark t))

(use-package company :ensure t
  :init
  (cond
   ((string-equal system-type "windows-nt")
    (progn
      (setq company-clang-executable "c:/cygwin64/bin/clang-5.0")
      )))
  :config 
  (setq-default company-show-numbers          1)
  (setq-default company-idle-delay            0)
  (setq-default company-minimum-prefix-length 1)
  (global-company-mode 1))

;; https://emacs.stackexchange.com/a/20024/21703
(defun my-jk ()
  (interactive)
  (let* ((initl-key ?j)
	 (final-key ?k)
	 (event (read-event nil nil 0.5)))
    (if event
	;; timeout met
	(if (and (characterp event) (= event final-key))
	    (evil-normal-state)
	  (insert initl-key)
	  (push event unread-command-events))
      ;; timeout exceeded
      (insert initl-key))))

(use-package evil :ensure t
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)

  ;; map insert mode
  (define-key evil-insert-state-map (kbd "j") 'my-jk)

  ;; map normal mode
  (define-key evil-normal-state-map ":" 'evil-repeat-find-char)
  (define-key evil-normal-state-map ";" 'evil-ex)
  ;; (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)

  ;; map visual mode
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

  ;; easy move between windows
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company rtags use-package popup fzf evil avy atom-one-dark-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
