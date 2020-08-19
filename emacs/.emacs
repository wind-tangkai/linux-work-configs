(setq confirm-kill-emacs 'y-or-n-p)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0) 

(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-default-style "ellemtel" c-basic-offset 4)

(global-set-key [C-f9] 'dired)

(setq split-height-threshold 128 
      split-width-threshold 256)
(defun my-split-window-sensibly (&optional window)
    "replacement slit-window-sensibly' function which prefers vertical splits"
    (interactive)
    (let ((window (or window (selected-window))))
        (or (and (window-splittable-p window t)
                 (with-selected-window window
                     (split-window-right)))
            (and (window-splittable-p window)
                 (with-selected-window window
                     (split-window-below))))))
(setq split-window-preferred-function 'my-split-window-sensibly)

;;packages
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

;;compilation settings
(setq compile-command "python ~/scons-local/scons.py -j9 -U mode=release")
(defun my-compile()
  "Save buffers and start compile"
  (interactive)
  (save-some-buffers t)
  (compile compile-command))
(global-set-key [f5] 'my-compile)
(global-set-key [f7] 'revert-buffer)
(setq compilation-scroll-output t)

;;locale coding 
(define-coding-system-alias 'UTF-8 'utf-8)
(setq locale-coding-system'utf-8) 
(prefer-coding-system'utf-8) 
(set-keyboard-coding-system'utf-8) 
(set-terminal-coding-system'utf-8) 
(set-selection-coding-system'utf-8) 
(set-clipboard-coding-system 'ctext) 
(set-buffer-file-coding-system 'utf-8) 

(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)
(setq switch-window-shortcut-appearance 'asciiart)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want wiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
;;(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;;completion
(add-hook 'after-init-hook 'global-company-mode)

;;avy
(global-set-key (kbd "C-c j") 'avy-goto-char)
(global-set-key (kbd "C-c ;") 'avy-goto-char-2)
(global-set-key (kbd "C-c w") 'avy-goto-word-1)
(global-set-key (kbd "C-c l") 'avy-goto-line)

;;show line number
(require 'linum)
(global-linum-mode t)
(global-hl-line-mode 1)
(setq column-number-mode t)
(setq line-number-mode t)

;;c++ related
(add-hook 'c++-mode-hook
      '(lambda ( )
         (c-toggle-hungry-state)))
(setq auto-mode-alist
     (cons '("\\.h\\'" . c++-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist
'("\\..pp\\'" . c++-mode))
(add-to-list 'auto-mode-alist
'("\\.ll\\'" . c++-mode))
(add-to-list 'auto-mode-alist
'("\\.yy\\'" . c++-mode))

;;c-mode related
(add-hook 'c-mode-hook
      '(lambda ( )
         (c-toggle-hungry-state)))

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;;parentheses settings
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;;transient-mark-mode
(setq transient-mark-mode t);

;;Enable EDE (Project Management) features
(global-ede-mode 1)

;;(require 'ansi-color)
;;(add-hook 'compilation-filter-hook
;;          '(lambda ()
;;             (let ((min (point-min-marker))
;;                   (max (point-max-marker)))
;;               (ansi-color-apply-on-region min max))))

(setq backup-by-copying-when-linked t)

(defun my-c-mode-hook ()
  (local-set-key "\C-xt" 'ff-find-other-file)
)
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;;magit
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
(global-set-key (kbd "C-c g") 'magit-status)

(put 'set-goal-column 'disabled nil)

(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))

(require 'jinja2-mode)
(add-to-list 'auto-mode-alist '("\\.jj2" . protobuf-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (counsel ivy switch-window protobuf-mode magit json-mode jinja2-mode epl company-c-headers avy all-the-icons-ivy))))
