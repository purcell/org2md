;;; org2md.el --- A plugin enhanced ox-gfm -*- lexical-binding: t -*-

;; Author: Jack LIU
;; Keywords: org, markdown, hexo

;;; Commentary:

;; This library can render a org file to a markdown file which can be rendered by HEXO.
;; based on the `ox-gfm' plugin.

;; It offers two functions:
;; jk/insert-date
;; jk/md-export

;;; Code:

(unless (package-installed-p 'ox-gfm)
  (package-refresh-contents)
  (package-install 'ox-gfm))


(defun org2md-insert-date ()
  "Insert current date."
  (interactive)
  (beginning-of-buffer)
  (insert "#+DATE: ")
  ;; (org-time-stamp t)
  (insert (format-time-string "%Y/%m/%d %T"))
  (insert "\n\n"))


(defun org2md-export-md ()
  "Export .org to .md which will be added Front-matter."
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
	    (beginning-of-buffer)
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
	      (replace-string ".." "")
	      (append-to-file nil t filename-dot-md)
	      (kill-this-buffer)
	      (switch-window-then-maximize))))))



(provide 'org2md)
;;; org2md.el ends here
