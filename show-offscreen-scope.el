
;;; Code:

(defun open-offscreen-scope-buffer ()
  (let* ((name (concat " *offscreen-scope: " (buffer-name) "*"))
         (buffer (get-buffer-create name)))
    (unless (get-buffer-window buffer)
      (save-selected-window
        (pop-to-buffer (current-buffer))
        (split-window-vertically 2)
        (switch-to-buffer buffer)
        (other-window 1)))
    buffer))

(defun current-line-string ()
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun copy-current-line-to (to-buffer)
  (let ((line (current-line-string)))
    (with-current-buffer to-buffer
      (goto-char 1)
      (insert line "\n"))))

(defun backward-line ()
  (forward-line -1))

(defun insert-offscreen-scope (to-buffer &optional new-window-start)
  (with-current-buffer to-buffer (erase-buffer))

  (save-excursion
    (goto-char (or new-window-start (window-start)))

    (let ((offscreen-scope-line-count 0))
      (while (not (or (= (current-indentation) 0)
                      (= (point) 1)))
        (let ((previous-indent (current-indentation)))
          (backward-line)
          (while (and (or (>= (current-indentation) previous-indent)
                          (= (line-beginning-position) (line-end-position))
                          (string-match-p "^[[:space:]]*{[[:space:]]*$" (current-line-string)))
                      (/= (point) 1))
            (backward-line))
          (when (< (current-indentation) previous-indent)
            (copy-current-line-to to-buffer)
            (incf offscreen-scope-line-count))))

      (save-selected-window
        (pop-to-buffer to-buffer)
        ;(if (> offscreen-scope-line-count 0)
        (let ((window-min-height 1))
          (fit-window-to-buffer))
          ;(kill-buffer-and-window))))))
        ))))

(defun show-offscreen-scope-after-scroll (window new-start)
  (let ((old-start (or (window-parameter window 'old-start) 0)))
    (when (and (not (string-match-p "\\*offscreen-scope:" (buffer-name)))
               (string-match-p "\\.el\\|\\.c\\|\\.py" (buffer-name))
               (or (/= new-start old-start) (/= (window-start window) old-start)))
      (remove-hook 'window-scroll-functions 'show-offscreen-scope-after-scroll)
      (show-offscreen-scope new-start)
      (add-hook 'window-scroll-functions 'show-offscreen-scope-after-scroll)
      (set-window-parameter window 'old-start new-start))))

(add-hook 'window-scroll-functions 'show-offscreen-scope-after-scroll)

(defun show-offscreen-scope (&optional new-start)
  (interactive)

  (let ((offscreen-scope-buffer (open-offscreen-scope-buffer)))
    (insert-offscreen-scope offscreen-scope-buffer new-start)))

(provide 'show-offscreen-scope)
;;; show-offscreen-scope.el ends here
