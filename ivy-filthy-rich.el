;;; ivy-filthy-rich.el --- Richer information for ivy candidates

;; Copyright (C) 2018 Yuan Fu

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Yuan Fu <casouri@gmail.com>
;; URL: https://github.com/casouri/ivy-filthy-rich
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ()

;;; Commentary:
;; 

;;; Code:

;;
;; Variabale & Customize

(defgroup ivy-filthy-rich nil
  "Customizations of ivy-filthy-rich"
  :prefix "ifrich-")

(defcustom ifrich-delimiter "    "
  "The delimiter between info groups."
  :type 'string
  :group 'ivy-filthy-rich)

(defcustom ifrich-left-pad ?\s
  "The left padding of `ifrich-delimiter'.
It is used when there are extra space.
The length of the pad has to be one.
If not, `ivy-filth-rich' will fallback to using space to pad.

Currently only support character, because `make-string' only accept that."
  :type 'character
  :group 'ivy-filthy-rich)

(defcustom ifrich-right-pad nil
  "The right padding of `ifrich-delimiter'.
It is used when there are extra space.
The length of the pad has to be one.
If not, `ivy-filth-rich' will fallback to using space to pad.

Currently only support character, because `make-string' only accept that."
  :type 'character
  :group 'ivy-filthy-rich)

(defcustom ifrich-max-length 150
  "The max length of one entry (one line on ivy buffer)."
  :type 'number
  :group 'ivy-filthy-rich)

(defcustom ifrich-format-func 'ifrich-format-candidate
  "The function that returns the final riched ivy candidate."
  :type 'symbol
  :group 'ivy-filthy-rich)

(defvar ifrich-default-switch-buffer-format
  "The default format for `ivy-switch-buffer'.
Format rule in info (C-h i)."
  '(((value . (lambda (candidate) '(candidate))) (prop . 0.2) (candidate . t))
    ((value . ifrich-get-major-mode) (prop . 0.2) (face . ((t (:foreground "red")))))
    ((value . ifrich-get-dir) (prop . 0.2) (face . ((t (:foreground "green")))))))

;; first entry is cosidered the candidate
;; the value function needs to return a list of possible values, sorted from longest to shortest
;; candiate info has to have a key 'candidate equal to t


;;
;; Function
;;

(defun ifrich--format-candidate (candidate format)
  "Format CANDIDATE into a rich candidate according to FORMAT."
  ;; replace functions with actual info string
  (let ((info-list nil)
        (entry-string-without-candidate "")
        (final-entry-string ""))
    (dolist (format-element format)
      (let ((func (alist-get 'value format-element)))
        ;; evaluate the function and replace it with returned value list
        (setf (alist-get 'value format-element) (funcall func candidate))
        ;; add the modified entry to new list
        (add-to-list 'info-list format-element t)))
    ;; trim entry
    (setq info-list (ifrich--trim-entry-to-max-length info-list))
    ;; format info-list into a entry for ivy to display
    ;; each info is an alist with key: value, prop, etc
    ;; TODO a separate func
    (dolist (info info-list)
      (unless (ifrich--is-candidate info)
        (let* ((value (car (alist-get 'value info)))
               (max-info-len (* ifrich-max-length (alist-get 'prop info)))
               ;; if value is shorter than max-info-length,
               ;; it needs to be padded.
               (extra-info-space (- max-info-len (length value)))
               ;; right padded before left
               (right-extra-count (if ifrich-left-pad ; delimiter's left is info's right
                                      (ceiling (/ extra-info-space 2))
                                    0))
               (left-extra-count (if ifrich-right-pad ; delimiter's right is info's left
                                     (- extra-info-space right-extra-count)
                                   0))
               (right-pad (if ifrich-left-pad
                              (make-string right-extra-count ifrich-left-pad )
                            ""))
               (left-pad (if ifrich-right-pad
                             (make-string left-extra-count ifrich-right-pad )
                           ""))
               (padded-value nil))
          (put-text-property 0 (length value) 'face (alist-get 'face info) value)
          (setq padded-value
                (concat left-pad value right-pad))
          (when (and (equal 0 right-extra-count) (equal 0 left-extra-count))
            (warn "`ifrich-left-pad' and `ifrich-right-pad' cannot both be nil!
Automatically setting left pad to space (align left).")
            (setq left-pad (make-string extra-info-space ?\s)))
          (setq entry-string-without-candidate (concat entry-string-without-candidate padded-value)))))
    ;; you thought that's all? naive!
    ;; TODO a seprate func
    (let* ((entry-without-candidate-max-len (- ifrich-max-length candidate))
           (actual-len (length entry-string-without-candidate))
           (max-minus-actual-length (- entry-without-candidate-max-len actual-len))
          (final-entry-without-candidate
           (if (< max-minus-actual-length 0)
               (let ((trimmed-entry (substring
                                     entry-string-without-candidate
                                     (+ max-minus-actual-length actual-len) (1- actual-len)))
                     (index 0)
                     (pad (char-to-string (or ifrich-right-pad ifrich-left-pad))))
                 (while (not (equal pad
                                    (substring-no-properties trimmed-entry)))
                   ;;TODO a separate func
                   (setq trimmed-entry (concat (substring trimmed-entry 0 index)
                                               pad
                                               (substring trimmed-entry (1+ index) (length trimmed-entry))))
                   (setq index (1+ index)))
                 (concat candidate trimmed-entry))
             final-entry-string))))))

;; test code
;; (let* ((format-element '((value . (lambda (str) "hahaha"))))
;;        (candidate "cand")
;;        (info-list ())
;;        (func (alist-get 'value format-element)))
;;   (setf (alist-get 'value format-element) (funcall func candidate))
;;   (add-to-list 'info-list format-element)
;;   info-list)

;; unused
(defun ifrich--calculate-entry-length (entry)
  "Calculate the length of the candidate ENTRY (info-alist) (one line in ivy buffer)."
  (let ((entry-len 0))
    (dolist (element entry)
      (setq entry-len (+ entry-len (length (car (alist-get 'value element))))))
    (+ entry-len (* (1- (length entry)) (length ifrich-delimiter)))))

(defun ifrich--is-candidate (info)
  "Check if the INFO element represents candidate."
  (if (alist-get 'candidate info)
      t
    nil))

(defun ifrich--trim-entry-to-max-length (info-list)
"Try to fit each info into its max-length and return the final info-list.
Each info's max length is calculated by `ifrich-max-length' x info proportion (prop).

Each info is an alist with key: value, prop, etc.

The function tries the first value (the longest) of info's value list,
if that doesn't fit, the funtion removes it and tries the formal second, now first
value in the value list. And this goes on.

If there is only one value left in the value list and it doesn't fit,
the function trims it from its tail and fit it to the max length.

In extrame cases this might return nil (when `ifrich-max-length' <= 0)"
  (if (<= ifrich-max-length 0)
      (progn
        (warn "ifrich-max-length has to be greater than 0! Current value is %s" ifrich-max-length)
        nil)
    ;; remove until fit
    ;; main logic starts here
    (dolist (info info-list)
      (unless (ifrich--is-candidate info)
        (let* ((value-list (alist-get 'value info-list))
               (first-value-len (length (car value-list)))
               (info-max-len (floor (* ifrich-max-length (alist-get 'prop info)))))
          ;; try next value until fit or only one value left
          (while (and (< info-max-len first-value-len)
                      (< 1 (length value-list)))
            (pop (alist-get 'value info-list))) ; info-list wouldn't change if pop value-list
          ;; only one left but still doesn't fit
          (when (< info-max-len first-value-len)
            (push (substring (pop (alist-get 'value info-list)) 0 (1- info-max-len))
                  (alist-get 'value info-list))))))
    info-list))


;;
;; Test
;;

(defun ifrich--calculate-entry-length-test ()
  "Test."
  (let ((entry '(((value . ("1234"))) ((value . ("1234"))) ((value . ("1234")))))
        (ifrich-delimiter "1"))
    (if (equal (ifrich--calculate-entry-length entry) 14)
        (message "pass")
      (message "ifrich-calculate-entry-length-test failed."))))

(defun ifrich--trim-entry-to-max-length-test ()
  "Test."
  (let ((ifrich-max-length 6)
        ;; FIXME
        (entry '(((value . ("1234"))) ((value . ("1234"))) ((value . ("1234"))))))
    (if (equal '(((value . ("1234")))) (ifrich--trim-entry-to-max-length entry))
        (message "pass")
      (message "ifrich-trim-entry-to-max-length-test 1 failed.")))
  (let ((ifrich-max-length -1)
        (entry '(((value . ("1234"))) ((value . ("1234"))) ((value . ("1234")))))
        (warning-minimum-level :emergency))
    (if (equal nil (ifrich--trim-entry-to-max-length entry))
        (message "pass")
      (message "ifrich-trim-entry-to-max-length-test 2 failed."))))
  

(defun ifrich--format-candidate-test ()
  "Test."
  (let ((format '(((value . (lambda (candidate) (list candidate))) (prop . 0.2) (candidate . t))
                 ((value . (lambda (candidate) '("looooooooong2" "short"))) (prop . 0.4) (face . ((t (:foreground "red")))))
                 ((value . (lambda (candidate) '("looooooooooong3" "short one"))) (prop . 0.4) (face . ((t (:foreground "green")))))))
         (ifrich-left-pad ?\s)
         (ifrich-right-pad nil)
         (ifrich-max-length 150))
    (message (ifrich--format-candidate "cand" format))
    ;; (if (equal (ifrich--format-candidate "cand" format) "")
    ;;     (message "pass")
    ;;   (message "ifrich--format-candidate-test 1 failed."))
    ))


(provide 'ivy-filthy-rich)

;;; ivy-filthy-rich.el ends here
