;;
;; package
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;
;; load each init files
;;
(load  "~/.emacs.d/settings.el")
(load "~/.emacs.d/global-packages.el")
(load "~/.emacs.d/prog.el")
(load "~/.emacs.d/productivity-tools.el")
(load "~/.emacs.d/dired.el")
(load "~/.emacs.d/misc.el")
