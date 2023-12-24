;;; bear.el --- Interact with Bear notes from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Samuel Cedarbaum

;; Author: Samuel Cedarbaum <scedarbaum@gmail.com>
;; Version: 0.1
;; Package-Requires: ((markdown-mode "2.6"))
;; Keywords: bear, notes, markdown
;; URL: https://github.com/cedarbaum/bear.el

;;; Commentary:

;; This package provides a major mode for interacting with Bear notes.

;;; Code:

(require 'markdown-mode)

(defcustom bear-application-data-url "~/Library/Group Containers/9K33E3U3T4.net.shinyfrog.bear/Application Data/"
  "URL for Bear's application data."
  :type 'directory
  :group 'bear)

(defcustom bear-cache-location (expand-file-name ".bear/" user-emacs-directory)
  "Path to the bear cache location."
  :type 'directory
  :group 'bear)

;;;###autoload
(defun bear-open-note ()
  "Prompt the user to select an option and return the corresponding ID."
  (interactive)
  (let* ((notes (bear-list-notes))
         (options (mapcar 'cdr notes)) ; Extract just the display text
         (selection (completing-read "Choose an option: " options nil t))
         (selected-id (car (rassoc (list selection) notes)))) ; Find the ID associated with the selection
    (bear--open-or-reload-note selected-id)))

(defun bear--ensure-cache-directory-exists ()
  "Ensure that the bear cache directory exists."
  (unless (file-exists-p bear-cache-location)
    (make-directory bear-cache-location t)))

(defun bear--get-db-url ()
  "Get the bear database URL."
  (expand-file-name "database.sqlite" bear-application-data-url))

(defun bear--get-db ()
  "Get the bear database."
  (let* ((db (sqlite-open (bear--get-db-url))))
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
         (text (mapconcat 'identity text-lines)))
    text))

(defun bear--get-pk-from-title (title)
  "Get the primary key of the note with the given TITLE."
  (let* ((note (sqlite-select (bear--get-db) (format "SELECT Z_PK FROM ZSFNOTE WHERE ZTITLE='%s'" title)))
         (pk (car (nth 0 note))))
    pk))

(defun bear--get-current-buffer-pk ()
  "Get the primary key of the current buffer."
  (let ((title (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (let ((note-pk (bear--get-pk-from-title title)))
      (if note-pk
          note-pk
        (message "No note found for title %s" title)))))

(defun bear--open-or-reload-note (note-pk &optional section)
  "Open the note with the given NOTE-PK.
Optional argument SECTION specifies a section to jump to."
  (let* ((title (bear--get-note-title note-pk))
         (text (bear--get-note-text note-pk))
         (file-name (format "%s.bearmd" title))
         (full-path (expand-file-name file-name bear-cache-location))
         (buffer (find-file-noselect full-path)))

    ;; Ensure that the cache directory exists
    (bear--ensure-cache-directory-exists)

    (with-current-buffer buffer
      ;; Set up the buffer content if the file is new or the note has changed
      (save-excursion
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert text)
        (when (fboundp 'format-all-buffer)
          (setq format-all-formatters '(("Markdown" prettier)))
          (format-all-buffer))
        (setq buffer-read-only t)
        (save-buffer))

      ;; Switch to bear-mode
      (bear-mode)

      ;; Jump to the section if specified
      (when section
        (goto-char (point-min))
        (if (re-search-forward (format "^#+ %s$" section) nil t)
            (beginning-of-line)
          (message "Section %s not found in note %s" section title))))

    ;; Switch to the buffer in the current window
    (switch-to-buffer buffer)))

(defun bear--parse-title-and-section (str)
  "Parse out the title and section from STR of format 'title/section'.
Returns a cons cell (title . section), where either part may be nil."
  (when (string-match "\\(.*?\\)\\(?:/\\(.*?\\)\\)?$" str)
    (let ((title (match-string 1 str))
          (section (match-string 2 str)))
      (cons (if (string= title "") nil title)
            (if (string= section "") nil section)))))

(defun bear--make-backlink-click-function (start end)
  "Create a click function for text between START and END."
  (lambda ()
    (interactive)
    (let* ((link (buffer-substring-no-properties start end))
           ;; Extract the title from link [[...]]))
           (title-and-section (bear--parse-title-and-section link))
           (title (car title-and-section))
           (section (cdr title-and-section))
           (note-pk (if title
                        (car (rassoc (list title) (bear-list-notes)))
                      (when section (bear--get-current-buffer-pk)))))
      (if note-pk
          (bear--open-or-reload-note note-pk section)
        (message "No note found for link %s" link)))))

(defun bear--fontify-clickable-backlinks (limit)
  "Search for clickable links up to LIMIT and make them clickable."
  (while (re-search-forward "\\(\\[\\[\\)\\(.*?\\)\\(\\]\\]\\)" limit t)
    (let* ((left-bracket-start (match-beginning 1))
           (left-bracket-end (match-end 1))
           (start (match-beginning 2))
           (end (match-end 2))
           (right-bracket-start (match-beginning 3))
           (right-bracket-end (match-end 3))
           ;; Markup part
           (mp (list 'invisible 'markdown-markup
                     'rear-nonsticky t
                     'font-lock-multiline t))
           (click-func (bear--make-backlink-click-function start end))
           (map (let ((map (make-sparse-keymap)))
                  (define-key map [mouse-1] click-func)
                  map))
           ;; URL part
           (up (list 'keymap map
                     'invisible 'markdown-markup
                     'mouse-face 'markdown-highlight-face
                     'font-lock-multiline t)))
      ;; [
      (add-text-properties left-bracket-start left-bracket-end mp)
      (add-face-text-property left-bracket-start left-bracket-end 'markdown-markup-face)

      ;; ]
      (add-text-properties right-bracket-start right-bracket-end mp)
      (add-face-text-property right-bracket-start right-bracket-end 'markdown-markup-face)

      ;; Link text
      (add-text-properties start end up)
      (add-face-text-property start end 'markdown-url-face))))

(defvar bear-mode-font-lock-keywords
  (append markdown-mode-font-lock-keywords
          '((bear--fontify-clickable-backlinks . nil))))

;; Define bear-mode as a derived mode of markdown-mode
;;;###autoload
(define-derived-mode bear-mode markdown-mode "Bear"
  "A mode derived from markdown-mode with read-only buffers."
  (setq font-lock-defaults '(bear-mode-font-lock-keywords))
  (setq buffer-read-only t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bearmd\\'" . bear-mode))

(provide 'bear)
;;; bear.el ends here
