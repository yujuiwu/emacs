;;
;; for chrome
;;
;;(add-to-list 'load-path "~/.emacs.d")
(require 'edit-server)
(edit-server-start)

;;
;; org-jira
;;
(setq jiralib-url "https://jira.realtek.com")

;;
;; geeknote
;;
(require 'geeknote)
(global-set-key (kbd "C-c g c") 'geeknote-create)
(global-set-key (kbd "C-c g e") 'geeknote-edit)
(global-set-key (kbd "C-c g f") 'geeknote-find)
(global-set-key (kbd "C-c g F") 'geeknote-find-tags)
(global-set-key (kbd "C-c g s") 'geeknote-show)
(global-set-key (kbd "C-c g r") 'geeknote-remove)
(global-set-key (kbd "C-c g m") 'geeknote-move)
(server-start)
