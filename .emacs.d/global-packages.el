;;
;; undo tree
;;
(global-undo-tree-mode)
(global-set-key (kbd "M-/") 'undo-tree-visualize)

;;
;; transparency
;;
(require 'alpha)
(global-set-key (kbd "C-M-)") 'transparency-increase)
(global-set-key (kbd "C-M-(") 'transparency-decrease)

;;
;; powerline
;;
(require 'powerline)
(powerline-default-theme)

;;
;; helm, ref https://tuhdo.github.io/helm-intro.html
;;
(require 'helm)
(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-X") 'execute-extended-command)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h o") 'helm-occur)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)

;;
;; ace-window
;;
(global-set-key (kbd "M-P") 'ace-window)

;;
;; ace-jump-mode
;;
(add-to-list 'load-path "which-folder-ace-jump-mode-file-in/")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;
;; fix highlight-symbol-at-point key binding
;;
(global-set-key (kbd "M-s h .") 'highlight-symbol-at-point)

;;
;; yasnippet
;;
;;(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;
;; ibuffer
;;
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;
;; pdf-tools, this takes really a long time
;;
;;(pdf-tools-install)

;;
;; magit, ref http://wikemacs.org/wiki/Magit
;;
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(magit-diff-added ((t (:background "black" :foreground "green3"))))
   '(magit-diff-removed ((t (:background "black" :foreground "red3")))))
