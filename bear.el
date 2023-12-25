;;; bear.el --- Interact with Bear notes from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Samuel Cedarbaum

;; Author: Samuel Cedarbaum <scedarbaum@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (markdown-mode "2.6"))
;; Keywords: bear, notes, markdown
;; URL: https://github.com/cedarbaum/bear.el

;;; Commentary:

;; This package provides a major mode for interacting with Bear notes.

;;; Code:

(require 'cl-lib)
(require 'markdown-mode)

(defcustom bear-application-data-url "~/Library/Group Containers/9K33E3U3T4.net.shinyfrog.bear/Application Data/"
  "URL for Bear's application data."
  :type 'directory
  :group 'bear)

(defcustom bear-format-function nil
  "Function to format the bear buffer contents."
  :type 'function
  :group 'bear)

(defcustom bear-readonly t
  "Whether to open notes in read-only mode."
  :type 'boolean
  :group 'bear)

(defvar-local bear--note-pk nil
  "The primary key of the note in the current buffer.")

;;; Interactive functions

;;;###autoload
(defun bear-open-note ()
  "Prompt the user to select a note and open it."
  (interactive)
  (let* ((notes (bear--list-notes))
         (titles (mapcar (lambda (note) (nth 2 note)) notes))
         (title-counts (make-hash-table :test 'equal)))

    ;; Count the occurrences of each title
    (dolist (title titles)
      (puthash title (1+ (gethash title title-counts 0)) title-counts))

    ;; Prompt the user to choose a note
    (let* ((options (mapcar (lambda (note)
                              (let ((title (nth 2 note)))
                                (if (> (gethash title title-counts) 1)
                                    (let* ((note-unique-id (nth 1 note)))
                                      (format "%s (%s)" title note-unique-id))
                                  title)))
                            notes))
           (selection (completing-read "Choose a note: " options nil t))
           (selected-note (cl-find-if (lambda (note)
                                        (let ((title (nth 2 note))
                                              (note-unique-id (nth 1 note)))
                                          (or (string= title selection)
                                              (string= (format "%s (%s)" title note-unique-id) selection))))
                                      notes)))
      (if selected-note
          (let* ((selected-note-pk (car selected-note))
                 (selected-note-unique-id (nth 1 selected-note)))
            (bear--open-note selected-note-pk nil selected-note-unique-id))
        (message "Could not open selected note %s" selection)))))

;;;###autoload
(defun bear-pull-note()
  "Pull the note from Bear."
  (interactive)
  (bear--assert-note-pk-exists)
  (let* ((note-pk bear--note-pk)
         (title (bear--get-note-title note-pk))
         (text (bear--get-note-text note-pk)))
    (save-excursion
      (let* ((buffer-read-only nil))
        (erase-buffer)
        (insert text)
        (when bear-format-function
          (funcall bear-format-function)))
      (message "Pulled note %s" title))))

;;;###autoload
(defun bear-push-note ()
  "Push the note to Bear."
  (interactive)
  (bear--assert-note-pk-exists)
  (let* ((note-pk bear--note-pk)
         (title (bear--get-curent-note-title))
         (text (buffer-string)))
    (bear--update-note note-pk title text)
    (message "Pushed note %s" title)))

;;;###autoload
(defun bear-create-note ()
  "Create a new note."
  (interactive)
  (bear--assert-can-write)
  (let* ((title (read-string "Title: ")))
    (let* ((unique-id (bear--generate-guid))
           (buffer (get-buffer-create (bear--get-buffer-name title unique-id))))
      (with-current-buffer buffer
        (bear-mode)
        (save-excursion
          (let* ((buffer-read-only nil))
            (erase-buffer)
            (insert (format "# %s" title))))

        (let* ((note-pk (bear--create-note title (buffer-string) unique-id)))
          (setq bear--note-pk note-pk)
          (message "Created note %s" title)))

      (switch-to-buffer buffer))))

;;; Bear DB configuration

(defun bear--get-db-url ()
  "Get the bear database URL."
  (expand-file-name "database.sqlite" bear-application-data-url))

(defvar bear--db nil
  "The bear database.")

(defun bear--get-db ()
  "Get the bear database."
  (unless bear--db
    (setq bear--db (sqlite-open (bear--get-db-url))))
  bear--db)

(defun bear--reset-db-connection ()
  "Reset the bear database connection."
  (when bear--db
    (sqlite-close bear--db)
    (setq bear--db nil)))

;;; Bear SQL functions

(defun bear--list-notes ()
  "Return a list of all notes in the database."
  (sqlite-select (bear--get-db) "SELECT Z_PK,ZUNIQUEIDENTIFIER,ZTITLE FROM ZSFNOTE WHERE ZTRASHED=0 AND ZARCHIVED=0 AND ZENCRYPTED=0"))

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

(defun bear--note-pk-exists-p (note-pk)
  "Return t if the note with the given NOTE-PK exists."
  (let* ((note (sqlite-select (bear--get-db) (format "SELECT Z_PK FROM ZSFNOTE WHERE Z_PK=%s" note-pk))))
    (not (null note))))

(defun bear--create-note (title text unique-id)
  "Create a new note with the given TITLE, TEXT, and UNIQUE-ID.
Returns the note's primary key."
  (bear--assert-can-write)
  (let* ((db (bear--get-db))
         (current-time (bear--core-data-timestamp)))
    (sqlite-execute db
                    "INSERT INTO ZSFNOTE (ZTITLE, ZTEXT, ZARCHIVED, ZENCRYPTED, ZHASFILES, ZHASIMAGES, ZHASSOURCECODE, ZLOCKED, ZORDER, ZPERMANENTLYDELETED, ZPINNED, ZSHOWNINTODAYWIDGET, ZSKIPSYNC, ZTODOCOMPLETED, ZTODOINCOMPLETED, ZTRASHED, ZVERSION, ZUNIQUEIDENTIFIER, ZCREATIONDATE, ZMODIFICATIONDATE) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                    (list title text 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 unique-id current-time current-time))
    (car (car (sqlite-select db "SELECT last_insert_rowid()")))))

(defun bear--update-note (note-pk title text)
  "Update the note with the given NOTE-PK with the given TITLE and TEXT."
  (bear--assert-can-write)
  (let* ((db (bear--get-db))
         (current-time (bear--core-data-timestamp)))
    (sqlite-execute db
                    "UPDATE ZSFNOTE SET ZTITLE=?, ZTEXT=?, ZMODIFICATIONDATE=? WHERE Z_PK=?"
                    (list title text current-time note-pk))))

;;; Bear buffer functions

(defun bear--open-note (note-pk unique-id &optional section)
  "Open the note with the given NOTE-PK.
Optional argument SECTION specifies a section to jump to.
Optional argument UNIQUE-ID specifies the unique ID of the note to display in the buffer."
  (let* ((title (bear--get-note-title note-pk))
         (text (bear--get-note-text note-pk))
         (buffer (get-buffer-create (bear--get-buffer-name title unique-id))))

    (with-current-buffer buffer
      ;; Switch to bear-mode
      (bear-mode)

      ;; Set up the buffer content if the file is new or the note has changed
      (save-excursion
        (let* ((buffer-read-only nil))
          (erase-buffer)
          (insert text)
          (when bear-format-function
            (funcall bear-format-function))))

      ;; Switch to the buffer in the current window
      (switch-to-buffer buffer)

      ;; Jump to the section if specified
      (when section
        (bear--jump-to-section section)))

    (setq bear--note-pk note-pk)))

(defun bear--jump-to-section (section)
  "Jump to the given SECTION in the current buffer."
  (goto-char (point-min))
  (if (re-search-forward (format "^#+ %s$" section) nil t)
      (beginning-of-line)
    (message "Section %s not found" section)))

(defun bear--get-curent-note-title ()
  "Return the title of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((title nil))
      (while (and (null title) (not (eobp)))
        (when (and (looking-at "^# +\\(.*[^[:space:]].*\\)$")
                   (not (string-empty-p (match-string 1))))
          (setq title (substring-no-properties (match-string 1))))
        (forward-line 1))
      title)))

(defun bear--get-backlink-target-note-pk (title)
  "Return the note-pk of the note with the given TITLE."
  (let* ((notes-with-title (cl-remove-if-not (lambda (note)
                                               (string= title (nth 2 note)))
                                             (bear--list-notes))))
    ;; If there's only one note with the given title, return it
    (if (= (length notes-with-title) 1)
        (car (car notes-with-title))
      ;; Let user choose which note to link to if it's ambiguous
      (let* ((options (mapcar (lambda (note)
                                (let* ((note-title (nth 2 note))
                                       (note-unique-id (nth 1 note)))
                                  (format "%s (%s)" note-title note-unique-id)))
                              notes-with-title))
             (selection (completing-read (format "Link could refer to multiple notes: " title) options nil t)))
        (car (cl-find-if (lambda (note)
                           (string= selection (format "%s (%s)" (nth 2 note) (nth 1 note))))
                         notes-with-title))))))

;;; Fontification

(defun bear--make-backlink-click-function (start end)
  "Create a click function for text between START and END."
  (lambda ()
    (interactive)
    (let* ((link (buffer-substring-no-properties start end))
           ;; Extract the title from link [[...]]))
           (title-and-section (bear--parse-title-and-section link))
           (title (car title-and-section))
           (section (cdr title-and-section))
           (target-note-pk (if title
                               (bear--get-backlink-target-note-pk title)
                             (when section
                               bear--note-pk))))
      (if target-note-pk
          (if (not (eq target-note-pk bear--note-pk))
              (bear--open-note target-note-pk nil section)
            (bear--jump-to-section section))
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

;;; Utility functions

(defun bear--assert-can-write ()
  "Assert that the bear mode is allowed to write."
  (when bear-readonly
    (error "Bear mode is read-only for safety.  You can disable this, but review the README first")))

(defun bear--assert-note-pk-exists ()
  "Assert that the current buffer has a note-pk."
  (when (or (null bear--note-pk)
            (not (bear--note-pk-exists-p bear--note-pk)))
    (error "No note-pk found for current buffer")))

(defun bear--core-data-timestamp ()
  "Return the number of seconds since January 1, 2001."
  (let* ((seconds-per-day 86400)
         (days-in-year 365)
         (leap-years 8)
         (non-leap-years 23)
         (seconds-since-epoch-to-2001 (+ (* leap-years (1+ days-in-year) seconds-per-day)
                                         (* non-leap-years days-in-year seconds-per-day)))
         (current-time (float-time)))
    (- current-time seconds-since-epoch-to-2001)))

(defun bear--generate-guid ()
  "Generate a GUID string in the format XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX."
  (format "%08X-%04X-%04X-%04X-%012X"
          (random (expt 16 8)) (random (expt 16 4))
          (random (expt 16 4)) (random (expt 16 4))
          (random (expt 16 12))))

(defun bear--get-buffer-name (title &optional unique-id)
  "Return the buffer name for a note with the given TITLE and optional UNIQUE-ID."
  (if unique-id
      (format "*bear-note-%s (%s)*" title unique-id)
    (format "*bear-note-%s*" title)))

(defun bear--parse-title-and-section (str)
  "Parse out the title and section from STR of format \\='title/section\\='.
Returns a cons cell (title . section), where either part may be nil."
  (when (string-match "\\(.*?\\)\\(?:/\\(.*?\\)\\)?$" str)
    (let ((title (match-string 1 str))
          (section (match-string 2 str)))
      (cons (if (string= title "") nil title)
            (if (string= section "") nil section)))))

;; Define bear-mode as a derived mode of markdown-mode
;;;###autoload
(define-derived-mode bear-mode markdown-mode "Bear"
  "A mode derived from markdown-mode with read-only buffers."
  (setq font-lock-defaults '(bear-mode-font-lock-keywords))
  (setq buffer-read-only bear-readonly))

(provide 'bear)
;;; bear.el ends here
