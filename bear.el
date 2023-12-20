;;; bear.el --- Interact with Bear notes from Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'markdown-mode)

(defcustom bear-db-url "~/Library/Group Containers/9K33E3U3T4.net.shinyfrog.bear/Application Data/database.sqlite"
  "URL for the bear database."
  :type 'string
  :group 'bear)

(defun bear-open-note ()
  "Prompt the user to select an option and return the corresponding ID."
  (interactive)
  (let* ((notes (bear-list-notes))
         (options (mapcar 'cdr notes)) ; Extract just the display text
         (selection (completing-read "Choose an option: " options nil t))
         (selected-id (car (rassoc (list selection) notes)))) ; Find the ID associated with the selection
    (bear--open-note selected-id)))

(defun bear--get-db ()
  "Get the bear database."
  (let* ((db (sqlite-open bear-db-url)))
    db))

(defun bear-list-notes ()
  "Return a list of all notes in the database."
  (sqlite-select (bear--get-db) "SELECT Z_PK,ZTITLE FROM ZSFNOTE WHERE ZTRASHED=0 AND ZARCHIVED=0 AND ZENCRYPTED=0"))

(defun bear--get-note-title (note-pk)
  "Get the title of the note with the given NOTE-PK."
  (let* ((note (sqlite-select (bear--get-db) (format "SELECT ZTITLE FROM ZSFNOTE WHERE Z_PK=%s" note-pk)))
         (title (car (nth 0 note))))
    title))

(defun bear--get-note-text (note-pk)
  "Get the text of the note with the given NOTE-PK."
  (let* ((note (sqlite-select (bear--get-db) (format "SELECT ZTEXT FROM ZSFNOTE WHERE Z_PK=%s" note-pk)))
         (text-lines (nth 0 note))
         (text (bear--concatenate-strings-with-newlines text-lines)))
    text))

(defun bear--open-note (note-pk)
  "Open the note with the given NOTE-PK."
  (let* ((title (bear--get-note-title note-pk))
         (text (bear--get-note-text note-pk)))
    (switch-to-buffer (get-buffer-create (format "*bear-note-%s*" title)))
    (bear-mode)
    (erase-buffer)
    (with-current-buffer (get-buffer (current-buffer))
      (let ((buffer-read-only nil))
        (insert text)
        (when (fboundp 'format-all-buffer)
          (setq format-all-formatters '(("Markdown" prettier)))
          (format-all-buffer))))))

;; Define bear-mode as a derived mode of markdown-mode
(define-derived-mode bear-mode markdown-mode "Bear"
  "A mode derived from markdown-mode with read-only buffers."
  (setq buffer-read-only t))


(provide 'bear)
;;; bear.el ends here
