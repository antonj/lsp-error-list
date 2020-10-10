(defgroup aj-lsp-error nil
  "list all lsp errors in compilation buffer"
  :group 'tools
  :group 'matching)

(defvar aj-lsp-error/file-column-pattern
  "^\\([[:digit:]]+\\):\\([[:digit:]]+\\):"
  "A regexp pattern to match line number and column number.")

(defun aj-lsp-error/compilation-match-filename ()
  (save-match-data
    (save-excursion
      (when (re-search-backward "^File: \\(.*\\)$" (point-min) t)
        (list (match-string 1))))))

(define-compilation-mode aj-lsp-error-mode "LSP-errors"
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(compilation-aj))
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (list (cons 'compilation-aj (list aj-lsp-error/file-column-pattern
                                         'aj-lsp-error/compilation-match-filename 1 2)))))

(define-key aj-lsp-error-mode-map (kbd "g") #'aj-lsp-error-buf)
(define-key aj-lsp-error-mode-map (kbd "p") #'compilation-previous-error)
(define-key aj-lsp-error-mode-map (kbd "n") #'compilation-next-error)
(define-key aj-lsp-error-mode-map (kbd "k") '(lambda () (interactive)
                                               (let (kill-buffer-query-functions) (kill-buffer))))

(defun aj-lsp-read-file-line (file line)
  (with-temp-buffer
    (insert-file-contents file)
    (save-excursion
      (goto-line line)
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))


(defun aj-lsp-error ()
  (interactive)
  ""
  (with-temp-buffer
    (save-excursion
      (let ((count 0))
        (maphash
         (lambda (file errors)
           (insert (format "File: %s\n" file))
           (dolist (e errors)
             (setq count (+ 1 count))
             (let* ((line (+ 1 (gethash "line" (gethash "start" (gethash "range" e)))))
                    (msg (gethash "message" e))
                    (column (gethash "character" (gethash "start" (gethash "range" e))))
                    (line-string (aj-lsp-read-file-line file line)))
               (insert (format "%s:%s:%s\n" line column line-string))
               (insert (concat "-- " (replace-regexp-in-string "\n" "\n-- " msg)))
               (insert "\n\n"))))
         (lsp-diagnostics))
        (cons count (buffer-string))))))

(defun aj-lsp-error-buf ()
  (interactive)
  ""
  (let ((buffer (get-buffer-create "*LSP Error List*")))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (errors (aj-lsp-error))
             (p (point))
             (num-errors (car errors))
             (str-errors (cdr errors)))
        (buffer-disable-undo nil)
        (erase-buffer)
        (insert (format "%d errors\n" num-errors)) ;; need this for wgrep
        (when (not (= num-errors 0))
          (insert str-errors))
        (set-buffer-modified-p nil)
        (aj-lsp-error-mode)
        (pop-to-buffer buffer '((display-buffer-reuse-window display-buffer-same-window)))
        (goto-char p)
        ))))

;; dev version
(aj-lsp-error-buf)

(defun aj-lsp-error-view ()
  (interactive)
  (aj-lsp-error-buf))

(global-set-key (kbd "M-g M-j") 'aj-lsp-error-view)

(require 'wgrep-ag)
;;;###autoload
(add-hook 'aj-lsp-error-mode-hook 'wgrep-ag-setup)
