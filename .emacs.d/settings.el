;;
;;add path for GNU global
;;
(setenv "PATH" (concat "/opt/local/bin" ":"
					   "/usr/local/bin" ":"
					   "/user/bin" ":" (getenv "PATH")))

(setq exec-path (append exec-path '("/opt/local/bin"
									"/usr/local/bin"
									"/usr/bin"
									"/user/bin")))

;;
;; theme
;;
(load-theme 'monokai t)

;;
;; emacs appearance customization
;;
;;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(column-number-mode)
(show-paren-mode)
(autopair-global-mode)
(autopair-global-mode)

;;
;; frame title
;;
(setq frame-title-format
	  '(buffer-file-name "%f"
						 (dired-directory dired-directory "%b")))

;;
;; org-mode for default
;;
(setq default-major-mode 'org-mode)

;;
;; open emacs in full screen
;;
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))
