(setq confirm-kill-emacs 'y-or-n-p)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0) 

(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-default-style "Linux")
(setq c-basic-offset 4)
(setq tab-width 4)
(setq c-default-style "ellemtel" c-basic-offset 4)

(global-set-key [C-f9] 'dired)

(setq package-list '(ace-jump-mode all-the-icons all-the-icons-ivy auto-complete counsel counsel-etags ctags-update epl ivy jinja2-mode json-mode magit tldr markdown-mode protobuf-mode go-mode go-rename golint company-go go-eldoc))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t)
(package-initialize))

;;fetch the list of packages available 
(unless package-archive-contents
    (package-refresh-contents))

;;install the missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
          (package-install package)))

;;compilation settings
(setq compile-command "python ~/scons-local/scons.py -j9 -U mode=release")
(defun my-compile()
  "Save buffers and start compile"
  (interactive)
  (save-some-buffers t)
  (compile compile-command))
(global-set-key [C-f5] 'compile)
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

(global-set-key  [C-left] 'windmove-left)
(global-set-key  [C-right] 'windmove-right)
(global-set-key  [C-up] 'windmove-up)
(global-set-key  [C-down] 'windmove-down)

;;show line number
(require 'linum)
(global-linum-mode t)
(global-hl-line-mode 1)
(setq column-number-mode t)
(setq line-number-mode t)

;;golang related
;;dependencies: `go get -u golang.org/x/tools/cmd/goimports && go get -u github.com/nsf/gocode && go get github.com/rogpeppe/godef`
(require 'company)
(require 'company-go)
(require 'go-eldoc)
(require 'go-rename)
(require 'golint)
(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq compilation-read-command nil))
(add-hook 'go-mode-hook 'go-mode-setup)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

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

;;set colors
(set-foreground-color "grey")
(set-background-color "black")
(set-cursor-color "grey")
(set-mouse-color "gold1")

;;parentheses settings
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;;transient-mark-mode
(setq transient-mark-mode t);

;;Enable EDE (Project Management) features
(global-ede-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(my-long-line-face ((((class color)) (:background "red1"))) t)
 '(my-tab-face ((((class color)) (:background "grey10"))) t)
 '(my-trailing-space-face ((((class color)) (:background "gray10"))) t))

(require 'ansi-color)
(add-hook 'compilation-filter-hook
          '(lambda ()
             (let ((min (point-min-marker))
                   (max (point-max-marker)))
               (ansi-color-apply-on-region min max))))

(setq backup-by-copying-when-linked t)

(defun my-c-mode-cedet-hook ()
  (local-set-key "\C-xt" 'ff-find-other-file)
)
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;;git related
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

;;ivy related
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(put 'set-goal-column 'disabled nil)

(ac-config-default)

(autoload
  'ace-jump-mode
  "ace-jump-mode" t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;markdown related
;;ref: `https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-markdown.el`
;;dependencies: `brew install grip`
(autoload 'markdown-mode "markdown-mode"
             "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
             "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(defun markdown-to-html ()
    (interactive)
    (let ((program "grip")
          (port "6419")
          (buffer "*gfm-to-html*"))

      ;; If process exists, kill it.
      (markdown-preview-kill-grip buffer)

      ;; Start a new grip process.
      (start-process program buffer program (buffer-file-name) port)
      (sleep-for 0.5) ; wait for process start
      (browse-url (format "http://localhost:%s/%s"
                          port
                          (file-name-nondirectory (buffer-file-name))))))
(global-set-key (kbd "C-c m")   'markdown-to-html)

(defun markdown-preview-kill-grip (&optional buffer)
    (interactive)
    ;; kill existed grip process.
    (let ((process (get-buffer-process (or buffer "*gfm-to-html*"))))
      (when process
        (kill-process process)
        (message "Process %s killed" process))))

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
    (tldr auto-complete-c-headers ctags-update company ace-jump-mode all-the-icons epl ivy counsel-etags all-the-icons-ivy counsel magit json-mode yaml-mode markdown-mode markdown-mode+ jinja2-mode auto-complete go-autocomplete))))
