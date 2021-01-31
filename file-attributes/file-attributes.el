;;;; Functions for displaying file attributes.


;; --------------------------------------------------------------------------------
;; FUNCTION 1: `add-line'
;; --------------------------------------------------------------------------------

(defun add-line (description attribute list)
  "Appends a DESCRIPTION and ATTRIBUTE to a LIST and returns it."

  ;; Check for nil
  (when (equal attribute nil)
    (setq attribute "nil"))

  ;; Convert attribute to a string
  (unless (stringp attribute)
    (setq attribute (number-to-string attribute)))

  ;; Append output line to list
  (setq list (cons description list))
  (setq list (cons attribute list)))


;; --------------------------------------------------------------------------------
;; FUNCTION 2: `formatted-file-attributes'
;; --------------------------------------------------------------------------------

(defun formatted-file-attributes (filename)
  "Output a formatted list of attributes for the passed FILENAME."
  (interactive "fFile name: ")

  (let ((attributes (file-attributes filename))
        (out-list ()))

    ;; File Type
    (setq out-list (add-line "File type"
        (file-attribute-type attributes) out-list))

    ;; Links
    (setq out-list (add-line "Links"
        (file-attribute-link-number attributes) out-list))

    ;; User ID
    (setq out-list (add-line "User ID"
        (file-attribute-user-id attributes) out-list))

    ;; Group ID
    (setq out-list (add-line "Group ID"
        (file-attribute-group-id attributes) out-list))

    ;; Last Access Time
    (setq out-list (add-line "Last Access Time"
        (format-time-string "%D" (file-attribute-access-time attributes))
                out-list))

    ;; Modification Time
    (setq out-list (add-line "Modification Time"
        (format-time-string "%D" (file-attribute-modification-time attributes))
                out-list))

    ;; Change Time
    (setq out-list (add-line "Change Time"
        (format-time-string "%D" (file-attribute-status-change-time attributes))
                out-list))

    ;; File Size
    (setq out-list (add-line "File size"
        (file-attribute-size attributes) out-list))

    ;; File Modes
    (setq out-list (add-line "File modes"
        (file-attribute-modes attributes) out-list))

    ;; Inode Number
    (setq out-list (add-line "Inode Number"
        (file-attribute-inode-number attributes) out-list))

    ;; Device Number
    (setq out-list (add-line "Device Number"
        (file-attribute-device-number attributes) out-list))

    (setq out-list (nreverse out-list))))


;; --------------------------------------------------------------------------------
;; FUNCTION 3: `file-attributes-to-messages'
;; --------------------------------------------------------------------------------

(defun file-attributes-to-messages (filename)
  "Output formatted file attributes of FILENAME to *Messages* buffer."
  (interactive "fFile name: ")

  (let ((out-list (formatted-file-attributes filename)))

    (message "%20s: %s\n\n" "Filename" filename)

    (while out-list
      (message "%20s: %s\n" (car out-list) (car (nthcdr 1 out-list)))
      (setq out-list (cdr (cdr out-list))))))
