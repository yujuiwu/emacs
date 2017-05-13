;;
;; Enable helm-gtags-mode
;;
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t))

(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-T") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-O") 'helm-gtags-find-tag-other-window)
     (define-key helm-gtags-mode-map (kbd "M-R") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-S") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;;
;; C indentation style
;;
(setq c-default-style "bsd")

;;
;; Set tab space
;;
(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode t)

;;
;; function-args
;;
(require 'function-args)
(fa-config-default)
;;(define-key c-mode-map  [(control tab)] 'moo-complete)
;;(define-key c++-mode-map  [(control tab)] 'moo-complete)
;;(define-key c-mode-map (kbd "M-o")  'fa-show)
;;(define-key c++-mode-map (kbd "M-o")  'fa-show)

;;
;; line number mode
;;
;;(global-linum-mode t)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'org-mode-hook 'linum-mode)

;;
;; which-function-mode
;;
;; Show the current function name in the header line
(which-function-mode)
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line, because it's mostly
      ;; invisible here anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))

;;
;; projectile
;;
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)

;;
;; column-enforce-mode
;;
(add-hook 'c-mode-hook 'column-enforce-mode)

;;
;; irony, ref https://github.com/Sarcasm/irony-mode#compilation-database
;;
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;;
;; c-eldoc-mode, doesn't seem to work
;;
(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;;
;; magit-svn
;;
(add-hook 'magit-mode-hook 'magit-svn-mode)
