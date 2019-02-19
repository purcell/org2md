;;; jk-org2md-hexo.el --- A plugin enhanced ox-gfm -*- lexical-binding: t -*-

;; Author: Jack Liu
;; Keywords: org, markdown, github

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


(defun jk/insert-date ()
  "Insert current date."
  (interactive)
  (beginning-of-buffer)
  (insert "#+DATE: ")
  ;; (org-time-stamp t)
  (insert (format-time-string "%Y/%m/%d %T"))
  (insert "\n\n"))


(defun jk/md-export ()
  "Export org to markdown which will be added Front-matter."
  (interactive)
  (save-buffer)	                                        ;; Save current buffer
  (setq fname (car (split-string (buffer-name) "\\."))) ;; if current buffer is "hello-world.org", FNAME will be "hello-world"
  (setq fdir "../_posts/")				;; Set exported markdown dir
  (setq fnamed (concat fdir fname ".md"))		;; "hello-world.md"
  (setq fnamel (split-string fname "-"))		;; Now, FNAMEL will be ("hello" "world")
  ;; Split the FNAMEL and join them with <space>, now FTITLE will be "hello world"
  (setq ftitle (concat "---\ntitle: "
		       (car fnamel) " "
		       (car (cdr fnamel)) " "
		       (car (cdr (cdr fnamel))) " "
		       (car (cdr (cdr (cdr fnamel)))) " "
		       (car (cdr (cdr (cdr (cdr fnamel))))) "\n"))
  (write-region ftitle nil fnamed) ;; Add FTITLE to a file named "hello-world.md"
  ;; Append date like "date: 2019/02/10 10:47:56" to "hello-world.md"
  (beginning-of-buffer)
  (forward-char 8)
  (let (p1 p2)
    (setq p1 (point))
    (end-of-line)
    (setq p2 (point))
    (append-to-file "date: " nil fnamed)
    (append-to-file p1 p2 fnamed)
    (append-to-file "\nupdated: " nil fnamed)
    (append-to-file (format-time-string "%Y/%m/%d %T") nil fnamed)
    (append-to-file "\n---\n\n" nil fnamed))
  (org-gfm-export-as-markdown)			 ;; Invoke gfm plugin to open a relative .md buffer
  (replace-string ".." "")			 ;; ! Just for hexo-blog's special img-show format...
  (append-to-file nil t fnamed)	 ;; Append the contents of "hello-world.md" to "hello-world.md" again
  (kill-this-buffer)
  (switch-window-then-maximize)) ;; Kill the "hello-world.md" generate by GFM plugin to keep you stay in current .org file


(provide 'jk-org2md-hexo)
;;; jk-org2md-hexo.el ends here
