(defgroup aj-lsp-errors nil
  "list all lsp errors in compilation buffer"
  :group 'tools
  :group 'matching)

(defvar aj-lsp-errors/file-column-pattern
  "^\\([[:digit:]]+\\):\\([[:digit:]]+\\):"
  "A regexp pattern to match line number and column number.")

(defun aj-lsp-errors/compilation-match-filename ()
  (save-match-data
    (save-excursion
      (when (re-search-backward "^File: \\(.*\\)$" (point-min) t)
        (list (match-string 1))))))

(define-compilation-mode aj-lsp-errors-mode "LSP-errors"
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(compilation-aj))
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (list (cons 'compilation-aj (list aj-lsp-errors/file-column-pattern
                                         'aj-lsp-errors/compilation-match-filename 1 2)))))

(define-key aj-lsp-errors-mode-map (kbd "p") #'compilation-previous-error)
(define-key aj-lsp-errors-mode-map (kbd "n") #'compilation-next-error)
(define-key aj-lsp-errors-mode-map (kbd "k") '(lambda () (interactive)
                                                (let (kill-buffer-query-functions) (kill-buffer))))

(defun aj-lsp-read-file-line (file line)
  (with-temp-buffer
    (insert-file-contents file)
    (save-excursion
      (goto-line line)
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))


(defun aj-lsp-errors ()
  (interactive)
  ""
  (with-temp-buffer
    (save-excursion
      (maphash
       (lambda (file errors)
         (insert (format "File: %s\n" file))
         (dolist (e errors)
           (let* ((line (+ 1 (gethash "line" (gethash "start" (gethash "range" e)))))
                  (msg (gethash "message" e))
                  (column (gethash "character" (gethash "start" (gethash "range" e))))
                  (line-string (aj-lsp-read-file-line file line)))
             (insert (format "%s:%s:%s\n" line column line-string))
             (insert (concat msg "\n\n")
                     ))
           ))
       (lsp-diagnostics)))
    (buffer-string)))


(defun aj-lsp-errors-buf ()
  (interactive)
  ""
  (let ((buffer (get-buffer-create "*aj-LSP Error List*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (buffer-disable-undo nil)
          (erase-buffer)
          (insert (aj-lsp-errors))
          (set-buffer-modified-p nil)
          ;;(ag-mode)
          (aj-lsp-errors-mode)
          (switch-to-buffer buffer)
          )))))

(aj-lsp-errors-buf)
