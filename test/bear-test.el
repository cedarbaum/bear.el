;;; bear-test.el --- Tests for bear.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Samuel Cedarbaum

;;; Commentary:

;; Tests for bear.el.

;;; Code:

(require 'ert)
(require 'bear)

(defvar bear--test-file-path
  (or load-file-name (buffer-file-name))
  "Path of this test file.")

(defun bear--test-init ()
  "Initialize bear for testing."
  (bear--reset-db-connection)
  (let ((current-file-directory (file-name-directory (or load-file-name
                                                         (buffer-file-name)
                                                         bear--test-file-path))))
    (setq bear-application-data-url (concat current-file-directory "Application Data"))))

(ert-deftest bear-list-notes-test ()
  "Test listing notes."
  (bear--test-init)
  (should (equal (bear--list-notes) '((6  "Note 1")
                                      (7  "Note 2")
                                      (8  "Note 3")))))

(ert-deftest bear-get-note-title-test ()
  "Test getting note title."
  (bear--test-init)
  (should (equal (bear--get-note-title 6) "Note 1")))

(ert-deftest bear-get-note-text-test ()
  "Test getting note text."
  (bear--test-init)
  (should (equal (bear--get-note-text 6) "# Note 1\n\n* Some text\n* Some more text")))

(provide 'bear-test)
;;; bear-test.el ends here
