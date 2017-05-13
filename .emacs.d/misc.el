(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;;
;; perspective
;;
(persp-mode)
(require 'persp-projectile)
(define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project)

;;
;; pcap-mode
;;
(add-to-list 'load-path "~/.emacs.d/elpa/pcap-mode-20161025.748")
(require 'pcap-mode)
(setq pcap-mode-tshark-executable "/usr/local/bin/tshark")

;;
;; fix gnupg
;;
(setf epa-pinentry-mode 'loopback)

;;
;; auto complete
;;
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;;
;; for exporting org to pdf
;;
(setenv "PATH" (concat "/Library/TeX/Root/bin/x86_64-darwin" ":"
					   (getenv "PATH")))
(setq exec-path (append exec-path '("/Library/TeX/Root/bin/x86_64-darwin")))

;;
;; nyan cat
;;
(nyan-mode 1)

;;
;; display time in mode line
;;
(display-time-mode t)

;;
;; To split window vertically by default
;; Ref: http://stackoverflow.com/a/21551216
;;
(setq split-height-threshold 0)
(setq split-width-threshold nil)

;;
;; Avoid using arrow keys for buffer switch
;;
(global-set-key (kbd "C-x P") 'previous-buffer)
(global-set-key (kbd "C-x N") 'next-buffer)

;;
;; golden ratio, ref https://github.com/roman/golden-ratio.el
;;
;;(require 'golden-ratio)
;;(golden-ratio-mode 1)

;;
;; for w3m, ref: http://sachachua.com/blog/2008/08/sweet-facebook-in-emacs/
;;
(setq w3m-use-cookies t)

;;
;; beacon mode, ref https://github.com/Malabarba/beacon
;;
(beacon-mode 1)

;;
;; wgrep, for editig grep buffer in helm
;;
(require 'wgrep)

;;
;; For copying large files in dired w/o blocking emacs
;; ref: https://truongtx.me/2013/04/02/emacs-async-file-copying-in-dired-using-rsync
;;
(defun tmtxt/dired-rsync (dest)
  (interactive
   ;; offer dwim target as the suggestion
   (list (expand-file-name (read-file-name "Rsync to:" (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files nil current-prefix-arg)))
	;; the rsync command
	(setq tmtxt/rsync-command "rsync -arvz --progress ")
	;; add all selected file names as arguments to the rsync command
    (dolist (file files)
	  (setq tmtxt/rsync-command
			(concat tmtxt/rsync-command
					(shell-quote-argument file)
					" ")))
	;; append the destination
	(setq tmtxt/rsync-command
		  (concat tmtxt/rsync-command
				  (shell-quote-argument dest)))
	;; run the async shell command
	(async-shell-command tmtxt/rsync-command "*rsync*")
	;; finally, switch to that window
	(other-window 1)))
;;; bind it to C-c C-r
(define-key dired-mode-map (kbd "C-c C-r") 'tmtxt/dired-rsync)
