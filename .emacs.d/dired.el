;;
;; ref: https://www.emacswiki.org/emacs/DiredPlus
;; Customize option ‘diredp-hide-details-initially-flag’ to ‘nil’. Or just toggle the display, by hitting `(’.
;;
;; ref: https://www.emacswiki.org/emacs/dired+.el
;;
;;  Hide/Show Details
;;  -----------------
;;
;;  Starting with Emacs 24.4, listing details are hidden by default.
;;  Note that this is different from the vanilla Emacs behavior, which
;;  is to show details by default.
;;
;;  Use `(' anytime to toggle this hiding.  You can use option
;;  `diredp-hide-details-initially-flag' to change the default/initial
;;  state.  See also option `diredp-hide-details-propagate-flag'.
;;
;;  NOTE: If you do not want to hide details initially then you must
;;        either (1) change `diredp-hide-details-initially-flag' using
;;        Customize (recommended) or (2) set it to `nil' (e.g., using
;;        `setq') *BEFORE* loading `dired+.el'.
;;
;;  If you have an Emacs version older than 24.4, you can use library
;;  `dired-details+.el' (plus `dired-details.el') to get similar
;;  behavior.
;;
(setq diredp-hide-details-initially-flag nil)

;;
;; ref: http://kuanyui.github.io/2014/06/21/dired-tutorial-and-essential-configs/
;;
(require 'dired)
(require 'dired-x)                   ;這行請記得加，不然無法使用隱藏檔案等功能。
(require 'dired+)                    ;請記得安裝 dired+，沒安裝的是笨蛋


;; 讓 Dired 有「多欄式」效果：用 C-x 3 在螢幕上開兩個 dired 視窗（如
;; 附圖），再來只要按 R 或 C（移動/複製檔案）時，會自動以另一個視窗為
;; 預設目標路徑。

(setq dired-dwim-target t)

;; 目錄排在檔案之前。
(defun dired-directory-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
(add-hook 'dired-after-readin-hook 'dired-directory-sort)

;; 按 q 回到上層目錄，並自動把 cursor 移動到前一個目錄處
(defun my-dired-backward ()
  "Go back to the parent directory (..), and the cursor will be moved to where
          the previous directory."
  (interactive)
  (let* ((DIR (buffer-name)))
    (if (equal DIR "*Find*")
        (quit-window t)
      (progn (find-alternate-file "..")
             (re-search-forward DIR nil :no-error)
             (revert-buffer)))))
(define-key dired-mode-map (kbd "q") 'my-dired-backward)  

;; 按 Enter 時 Dired 時不會一直開新的 Dired buffer（按 Enter 時只用同一個 Dired 開目錄）
(defun dired-my-find-alternate-file ()
  (interactive)
  (if (file-regular-p (dired-get-filename))
      (dired-find-file)
    (dired-find-alternate-file)))
(define-key dired-mode-map (kbd "RET") 'dired-my-find-alternate-file) ; 按 Enter 開檔案
(put 'dired-find-alternate-file 'disabled nil) ; 避免 Dired 問你一些囉唆的問題

;; ;;自動隱藏以.開頭的檔案（使用 C-x M-o 顯示/隱藏）
;; (setq dired-omit-files "^\\...+$")
;; ;; Dired Omit 加強:
;; ;; 簡單來說，這個能夠紀錄下目前的「隱藏狀態」，所以當你按
;; ;; C-x M-o 隱藏以.為開頭的檔案後，即使到了不同目錄下，以.開頭的檔案
;; ;; 依舊是處於隱藏狀態，直到你重新按 C-x M-o 為止。
;; (defvar v-dired-omit t
;;   "If dired-omit-mode enabled by default. Don't setq me.")
;; (defun dired-omit-and-remember ()
;;   "This function is a small enhancement for `dired-omit-mode', which will
;;         \"remember\" omit state across Dired buffers."
;;   (interactive)
;;   (setq v-dired-omit (not v-dired-omit))
;;   (dired-omit-auto-apply)
;;   (revert-buffer))
;; (defun dired-omit-auto-apply ()
;;   (setq dired-omit-mode v-dired-omit))
;; (define-key dired-mode-map (kbd "C-x M-o") 'dired-omit-and-remember)
;; (add-hook 'dired-mode-hook 'dired-omit-auto-apply)

;;使用 KB, MB 等方式顯示檔案大小（這個應該是 Unix 限定...Windows 我不
;;知該怎麼辦）。
(setq dired-listing-switches "-alh")

;; ;; 和 KDE 的 Dolphin 一樣的檔案名過濾器，按 C-i 使用。 (by letoh)
;; (defun dired-show-only (regexp)
;;   (interactive "sFiles to show (regexp): ")
;;   (dired-mark-files-regexp regexp)
;;   (dired-toggle-marks)
;;   (dired-do-kill-lines))
;; (define-key dired-mode-map (kbd "C-i") 'dired-show-only)

;; 遞迴拷貝/複製檔案時的確認訊息設定
(setq dired-recursive-copies  'always) ; 拷貝檔案；「always」 表示永不詢問。
(setq dired-recursive-deletes 'top) ; 刪除檔案：「top」表示同一批檔案只詢問一次。
;; ;; M-Enter 呼叫外部程式（此處是透過 `kde-open`）來開啟檔案，如果你不是
;; ;; 用 KDE，可以改成 xdg-open 之類的。Windows 我不知該怎麼辦啦啦啦。
;; (defun dired-open-file-with-external-program ()
;;   "Open file with external program in dired"
;;   (interactive)
;;   (let* ((file (dired-get-filename nil t)))
;;     (message "Opening %s..." file)
;;     (call-process "kde-open" nil 0 nil file)
;;     (message "Opening %s done" file)))
;; (define-key dired-mode-map (kbd "M-RET") 'dired-open-file-with-external-program)
;; ;; 在 Dired 下 C-x C-j 使用`kde-open`等外部程式開啟「當前目錄」
;; (defun open-current-directory-with-external-program ()
;;   "Open current directory with external program."
;;   (interactive)
;;   (call-process "kde-open" nil 0 nil (file-truename default-directory)))
;; (define-key dired-mode-map (kbd "C-x C-j") 'open-current-directory-with-external-program)co

;; 使用 f 搜尋目前目錄（這個部份可能也是 Unix 限定）
(define-key dired-mode-map "f" 'dired-find-name-in-current-directory)
(defun dired-find-name-in-current-directory ()
  (interactive)
  (find-name-dired default-directory
                   (format "*%s*" (read-from-minibuffer "Pattern: ")))
  (set-buffer-multibyte t))
(setq find-name-arg "-iname")
;; 修正*Find*裡的中文亂碼問題
(setq find-ls-option '("-print0 | xargs -0 ls -ald" . ""))

;; ;; 手動開系統的外接硬碟掛載目錄很麻煩，乾脆弄個快速鍵，C-c m 直接開
;; ;; /var/rum/media（如果你的系統掛載路徑不在這，請自行修改）
;; (defun dired-open-mounted-media-dir ()
;;   (interactive)
;;   (find-file "/var/run/media/"))
;; (define-key dired-mode-map (kbd "C-c m") 'dired-open-mounted-media-dir)

;; 按 s 排序檔案，會先問你要根據什麼屬性排序，而且紀錄下排序狀態，不會
;; 跨 buffer 就不見了。
(defun dired-sort-size ()
  "Dired sort by size."
  (interactive) (dired-sort-other (concat dired-listing-switches "S")))
(defun dired-sort-extension ()
  "Dired sort by extension."
  (interactive) (dired-sort-other (concat dired-listing-switches "X")))
(defun dired-sort-ctime ()
  "Dired sort by create time."
  (interactive) (dired-sort-other (concat dired-listing-switches "ct")))
(defun dired-sort-utime ()
  "Dired sort by access time."
  (interactive) (dired-sort-other (concat dired-listing-switches "ut")))
(defun dired-sort-time ()
  "Dired sort by time."
  (interactive) (dired-sort-other (concat dired-listing-switches "t")))
(defun dired-sort-name ()
  "Dired sort by name."
  (interactive) (dired-sort-other (concat dired-listing-switches "")))
(defvar v-dired-sort 'name)
(defun dired-sort-and-remember ()
  ""
  (interactive)
  (setq v-dired-sort
        (intern
         (completing-read "Sort by: " '(name size extension ctime utime time) nil t
                          (cond ((eq v-dired-sort 'name) "time")
                                ((eq v-dired-sort 'time) "name")
                                ((eq v-dired-sort 'size) "name")
                                (t nil)))))
  (dired-sort-auto-apply))
(defun dired-sort-auto-apply ()
  (cond ((eq v-dired-sort 'name) (dired-sort-name))
        ((eq v-dired-sort 'size) (dired-sort-size))
        ((eq v-dired-sort 'extenstion) (dired-sort-extenstion))
        ((eq v-dired-sort 'ctime) (dired-sort-ctime))
        ((eq v-dired-sort 'utime) (dired-sort-utime))
        ((eq v-dired-sort 'time) (dired-sort-time))))
(add-hook 'dired-mode-hook 'dired-sort-auto-apply)
(define-key dired-mode-map "s" 'dired-sort-and-remember)

;; ;; 看動畫很方便 ˊ・ω・ˋ 按 M-a 把檔案加入 SMPlayer 的播放清單中。
;; (defun dired-add-to-smplayer-playlist ()
;;   "Add a multimedia file or all multimedia files under a directory into SMPlayer's playlist via Dired."
;;   (interactive)
;;   (require 'cl)
;;   (let* ((PATTERN "\\(\\.mp4\\|\\.flv\\|\\.rmvb\\|\\.mkv\\|\\.avi\\|\\.rm\\|\\.mp3\\|\\.wav\\|\\.wma\\|\\.m4a\\|\\.mpeg\\|\\.aac\\|\\.ogg\\|\\.flac\\|\\.ape\\|\\.mp2\\|\\.wmv\\|.m3u\\|.webm\\)$")
;;          (FILE (dired-get-filename nil t)))
;;     (if (file-directory-p FILE) ;if it's a dir.
;;         (let* ((FILE_LIST (directory-files FILE t PATTERN))
;;                (n 0)
;;                s_FILE_LIST)
;;           (dolist (x FILE_LIST)
;;             (if (not (or (equal x ".") (equal x "..")))
;;                 (setq s_FILE_LIST (concat s_FILE_LIST "'" x "' ")))
;;             (setq n (1+ n)))
;;           (message "Opening %s files..." n)
;;           (call-process-shell-command "smplayer -add-to-playlist" nil nil nil (format "%s &" s_FILE_LIST)))
;;       (if (string-match PATTERN FILE)   ;if it's a file
;;           (call-process "smplayer" nil 0 nil "-add-to-playlist" FILE)
;;         (message "This is not a supported audio or video file."))))
;;   (dired-next-line 1))
;; (define-key dired-mode-map (kbd "M-a") 'dired-add-to-smplayer-playlist)
