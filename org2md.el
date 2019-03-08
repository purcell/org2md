;;; org2md.el --- Export ".org" to Hexo's ".md". -*- lexical-binding: t -*-

;; Author: Jack LIU <loveminimal@outlook.com>
;; Version: 0.1
;; Package-Requires: ((ox-gfm "1.0"))
;; Keywords: org, markdown, hexo
;; URL: https://github.com/loveminimal/org2md


;;; Commentary:

;; Hexo is a static blog generator, which just render specific format ".md" file.  So
;; you need `ox-gfm' to conform it.  And
;; this package can achieve this goal, which will auto add a Front-matter of exported markdown file.  Also,
;; it sloves the problem of images showwing.

;; It offers two functions:
;; 1. org2md-insert-date
;; 2. org2md-export-md

;; Note that, you must execute `org2md-insert-date' one time BEFORE executing `org2md-export-md'.
;; It is owing to that `org2md-export-md' is depending on a time-string offered by `org2md-insert-date'.


;;; Code:

(defun org2md-insert-date ()
  "Insert current date.
It will add a time-string at beginning of buffer, e.g. '#+DATE: 2019/03/08 09:51:11'."
  (interactive)
  (goto-char (point-min))
  (insert "#+DATE: ")
  ;; (org-time-stamp t)
  (insert (format-time-string "%Y/%m/%d %T"))
  (insert "\n\n"))


(defun org2md-export-md ()
  "Export '.org' to '.md' which will be added Front-matter.
Thus hexo can render it."
  (interactive)
  (save-buffer)
  (let ((filename (car (split-string (buffer-name) "\\.")))
	(filedir "../_posts/"))
	
	(let ((filename-dot-md (concat filedir filename ".md"))
	      (filename-list (split-string filename "-")))
	  (let ((file-title (concat "---\ntitle: "
				  (car filename-list) " "
				  (car (cdr filename-list)) " "
				  (car (cdr (cdr filename-list))) " "
				  (car (cdr (cdr (cdr filename-list)))) " "
				  (car (cdr (cdr (cdr (cdr filename-list))))) " "
				  (car (cdr (cdr (cdr (cdr (cdr filename-list)))))) " "
				  (car (cdr (cdr (cdr (cdr (cdr (cdr filename-list))))))) " "
				  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr filename-list)))))))) "\n")))
	    (write-region file-title nil filename-dot-md)
	    (goto-char (point-min))
	    (forward-char 8)
	    (let (p1 p2)
	      (setq p1 (point))
	      (end-of-line)
	      (setq p2 (point))
	      (append-to-file "date: " nil filename-dot-md)
	      (append-to-file p1 p2 filename-dot-md)
	      (append-to-file "\nupdated: " nil filename-dot-md)
	      (append-to-file (format-time-string "%Y/%m/%d %T") nil filename-dot-md)
	      (append-to-file "\n---\n\n" nil filename-dot-md)
	      (org-gfm-export-as-markdown)
	      (while (search-forward ".." nil t)
		(replace-match "" nil t))
	      (append-to-file nil t filename-dot-md)
	      (kill-this-buffer)
	      (switch-window-then-maximize))))))



(provide 'org2md)
;;; org2md.el ends here
