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
;; Package-Requires: ((emacs "25") (ivy "0.8.0"))

;;; Commentary:
;;  To enable this package, run
;; (ivy-filthy-rich-mode)
;;
;; For mor information, please read README.org.

;;; Code:

;;
;;; Variabale & Customize

(defgroup ivy-filthy-rich nil
  "Customizations of ivy-filthy-rich"
  :prefix "ifrich-"
  :group 'ivy-filthy-rich)

(defcustom ifrich-padding ?\s
  "The padding of `ifrich-delimiter'.
It is used when there are extra space.
The length of the pad has to be one.
If not, `ivy-filth-rich' will fallback to using space to pad.

Currently only support character, because `make-string' only accept that."
  :type 'character
  :group 'ivy-filthy-rich)

(defcustom ifrich-pad-side 'right
  "The side which padding is pad to.
Either left or right.

Left means align right,
right means align left."
  :type 'symbol
  :group 'ivy-filthy-rich)

(defcustom ifrich-max-length 0
  "The max length of one entry (one line on ivy buffer).
If it is zero, the max-length is (1- (frame-width))"
  :type 'number
  :group 'ivy-filthy-rich)

(defcustom ifrich-format-func 'ifrich-format-candidate
  "The function that returns the final riched ivy candidate."
  :type 'symbol
  :group 'ivy-filthy-rich)

;;
;;; Default format
;;

;;;; Ivy default transformers

;; counsel-find-file ivy-read-file-transformer
;; read-file-name-internal ivy-read-file-transformer
;; ivy-switch-buffer ivy-switch-buffer-transformer
;; internal-complete-buffer ivy-switch-buffer-transformer
;; counsel-describe-variable counsel-describe-variable-transformer
;; counsel-describe-function counsel-describe-function-transformer
;; counsel-M-x counsel-M-x-transformer
;; counsel-git-grep counsel-git-grep-transformer
;; counsel-ag counsel-git-grep-transformer
;; counsel-rg counsel-git-grep-transformer


;; the value function needs to return a list of possible values, sorted from longest to shortest
;; candiate info has to have a key 'candidate equal to t

(defvar ifrich-default-switch-buffer-format
  '(((value . (lambda (candidate) (list (ivy-switch-buffer-transformer candidate)))) (prop . 0.2) (candidate . t))
    ((value . ifrich--get-major-mode) (prop . 0.2) (face . (:foreground "#61AFEF")))
    ((value . ifrich--get-dir) (prop . 0.6) (face . (:foreground "#98C379"))))
  "The default format for `ivy-switch-buffer'.
Format rule in info (C-h i).")

(defvar ifrich-default-describe-function-format
  '(((value . (lambda (candidate) (list (counsel-describe-function-transformer candidate)))) (prop . 0.3) (candidate . t))
    ((value . ifrich--get-doc) (prop . 0.6) (face . (:foreground "#61AFEF"))))
  "The default format for `counsel-describe-function'.
Format rule in info (C-h i).")

(defvar ifrich-default-M-x-format 
  '(((value . (lambda (candidate) (list (counsel-M-x-transformer candidate)))) (prop . 0.3) (candidate . t))
    ((value . ifrich--get-doc) (prop . 0.6) (face . (:foreground "#61AFEF"))))
  "The default format for `counsel-M-x'.
Format rule in info (C-h i).")

(defvar ifrich-default-describe-variable-format
  '(((value . (lambda (candidate) (list candidate))) (prop . 0.3) (candidate . t))
    ((value . ifrich--get-doc-property) (prop . 0.6) (face . (:foreground "#61AFEF"))))
  "The default format for `counsel-describe-variable'.
Format rule in info (C-h i).")

(defvar ifrich-default-describe-face-format
  '(((value . (lambda (candidate) (list candidate))) (prop . 0.3) (candidate . t))
    ((value . ifrich--get-face) (prop . 0.7)))
  "The default format for `counsel-faces'.
Format rule in info (C-h i).")

;;
;;; Info Function (Return info string list, used in format)
;;

(defun ifrich--get-major-mode (candidate)
  "Return major mode of buffer (CANDIDATE)."
  (let ((buffer (get-buffer candidate)))
    (if buffer
        (list (substring-no-properties (symbol-name (buffer-local-value 'major-mode buffer))))
      '(""))))


(defun ifrich--get-dir (candidate)
  "Return directory of buffer (CANDIDATE)."
  (let* ((buffer (get-buffer candidate))
         (dir (when buffer
                (buffer-local-value 'default-directory buffer))))
    (if buffer
      (list dir
            (file-name-nondirectory (directory-file-name dir)))
      '(""))))

(defun ifrich--get-doc (candidate)
  "Return the first sentense of the documentation of CANDIDATE as a symbol."
  (let ((doc (or (documentation (intern candidate)) "")))
    (list (or (ifrich--get-doc-summary doc) ""))))

(defun ifrich--get-doc-property (candidate)
  "Return the first sentense of the documentation of CANDIDATE as a symbol."
  (let ((doc (or (documentation-property (intern candidate) 'variable-documentation) "")))
    (list (or (ifrich--get-doc-summary doc) ""))))

(defun ifrich--get-face (candidate)
  "Return a test string with face CANDIDATE applied."
  (let* ((doc (or (face-documentation (intern candidate)) ""))
         (demo (or (ifrich--get-doc-summary doc)
                   "I CAN'T GO ON LIKE THIS -- LOSING A BILLION DOLLARS A MINUTE! I'LL BE BROKE IN 600 YEARS!")))
    (list (propertize demo 'face (intern candidate)))))

(defun ifrich--get-doc-summary (doc)
  "Return the first sentence of DOC. return nil if not exist."
  (if (string-match "\\(^.+?\\.\\)\n" doc)
      (match-string 1 doc)
    (if (string-match "^.+?\\." doc)
        (match-string 0 doc)
      nil)))

;;
;;; Deploy function
;;

(defun ifrich-set-function ()
  "Set transform functions."
  (ivy-set-display-transformer 'ivy-switch-buffer          (lambda (candidate) (ifrich--format-candidate candidate ifrich-default-switch-buffer-format)))
  (ivy-set-display-transformer 'counsel-describe-function  (lambda (candidate) (ifrich--format-candidate candidate ifrich-default-describe-function-format)))
  (ivy-set-display-transformer 'counsel-describe-variable  (lambda (candidate) (ifrich--format-candidate candidate ifrich-default-describe-variable-format)))
  (ivy-set-display-transformer 'counsel-M-x                (lambda (candidate) (ifrich--format-candidate candidate ifrich-default-M-x-format)))
  (ivy-set-display-transformer 'counsel-describe-face      (lambda (candidate) (ifrich--format-candidate candidate ifrich-default-describe-face-format)))
  )

(define-minor-mode ivy-filthy-rich-mode
  "A global minor mode that adds information to ivy candidates. I'm F****** Rich."
  :lighter "IFRich"
  :global t
  (if ivy-filthy-rich-mode
      (progn
        (ivy-set-display-transformer 'ivy-switch-buffer          (lambda (candidate) (ifrich--format-candidate candidate ifrich-default-switch-buffer-format)))
        (ivy-set-display-transformer 'counsel-describe-function  (lambda (candidate) (ifrich--format-candidate candidate ifrich-default-describe-function-format)))
        (ivy-set-display-transformer 'counsel-describe-variable  (lambda (candidate) (ifrich--format-candidate candidate ifrich-default-describe-variable-format)))
        (ivy-set-display-transformer 'counsel-M-x                (lambda (candidate) (ifrich--format-candidate candidate ifrich-default-M-x-format)))
        (ivy-set-display-transformer 'counsel-describe-face      (lambda (candidate) (ifrich--format-candidate candidate ifrich-default-describe-face-format))))
    (ivy-set-display-transformer 'ivy-switch-buffer          nil)
    (ivy-set-display-transformer 'counsel-describe-function  nil)
    (ivy-set-display-transformer 'counsel-describe-variable  nil)
    (ivy-set-display-transformer 'counsel-M-x                nil)
    (ivy-set-display-transformer 'counsel-describe-face      nil)
    ))

;;
;;; Logic Function
;;

(defun ifrich--format-candidate (candidate format)
  "Format CANDIDATE into a rich candidate according to FORMAT."
  ;; 1. replace functions with actual info string
  (let ((info-list ())
        (entry-sequence ())
        (format (copy-tree format))
        (ifrich-max-length (when (equal 0 ifrich-max-length)
                             (1- (frame-width)))))
    (when (sequencep candidate)
      (setq candidate (substring-no-properties candidate)))
    (dolist (format-element format)
      (let ((func (alist-get 'value format-element)))
        ;; evaluate the function and replace it with returned value list
        (setf (alist-get 'value format-element) (funcall func candidate))
        ;; add the modified entry to new list
        (add-to-list 'info-list format-element t)))
    ;; 2. trim each part(info) of entry to it's planned max length (* prop ifrich-max-length)
    (setq info-list (ifrich--trim-entry-to-max-length info-list))
    ;; 3. format info-list into a sequence of strings to be concated
    ;; 4. sequence to string
    ;; each info is an alist with key: value, prop, etc
    (apply 'ifrich--concat-entry-sequence (ifrich--format-to-sequence info-list))))

(defun ifrich--trim-entry-to-max-length (info-list)
"Try to fit each info into its max-length and return the final INFO-LIST.
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
        (let ((value-list (alist-get 'value info))
              (info-max-len (floor (* (alist-get 'prop info) ifrich-max-length))))
          ;; try next value until fit or only one value left
          (while (and (< info-max-len (length (car value-list)))
                      (< 1 (length value-list)))
            (pop (alist-get 'value info)) ; info-list wouldn't change if pop value-list
            (pop value-list)) ; pop value-list to examine next value
          ;; only one left but still doesn't fit
          (when (< info-max-len (length (car value-list)))
            (push (substring (pop (alist-get 'value info)) 0 (1- info-max-len))
                  (alist-get 'value info))))))
    info-list))

(defun ifrich--format-to-sequence (info-list)
  "Turn a format (INFO-LIST) into a sequence of strings.
The first step replaced the functions in format with a list of possible strings.
The second step removes those possible strings that are too long.
This function colorize the string and add paddings.
Then all the fourth function to join that sequence into one string and return that.

The sequence is a list of colorized strings and paddings
For example, \(left-pad candidate right-pad left-pad value-1 right-pad\)
\(\"\" \"candddddddddidate\" \"      \" \"\" \"value 1\" \"       \"\)
Note that all strings have properties

Return \(entry-sequence candidate-index candidate-planned-len\)."
  (let ((candidate-planned-len 0)
        (candidate-index 0)
        (index 0)
        (entry-sequence ()))
    (dolist (info info-list)
      ;; (print info-list)
      (let* ((value (car (alist-get 'value info)))
             (max-info-len (floor (* ifrich-max-length (alist-get 'prop info))))
             ;; if value is shorter than max-info-length,
             ;; it needs to be padded.
             (extra-info-space (- max-info-len (length value)))
             ;; make sure it is >= 0
             (extra-info-space (if (> extra-info-space 0)
                                   extra-info-space
                                 0))
             ;; right padded before left
             (pad (make-string extra-info-space ifrich-padding))
             (left-pad (if (equal 'left ifrich-pad-side) pad ""))
             (right-pad (if (equal 'right ifrich-pad-side) pad ""))
             (value-with-pad ()))
        ;; coloring
        (let ((face-spec (alist-get 'face info)))
          (when face-spec
            (setq value (propertize value 'face face-spec))))
        ;; padding
        (setq value-with-pad
              (list left-pad value right-pad))
        ;; record candidate spec
        (when (ifrich--is-candidate info)
          (setq candidate-index index)
          (setq candidate-planned-len max-info-len))
        ;; add to entry
        (setq entry-sequence (append entry-sequence value-with-pad))
        (setq index (1+ index))))
    ;; entry-sequence is a list of form: (left-pad value right-pad left-pad value right-pad))
    (list entry-sequence candidate-index candidate-planned-len)))

(defun ifrich--concat-entry-sequence (seq candidate-index candidate-planned-length)
  "Concat all the elements in entry sequence SEQ together.
CANDIDATE-INDEX is the index of candidate if you consider left-pad, value and right-pad one element.
In another word, index of left-pad of candidate in SEQ is (* 3 CANDIDATE-INDEX),
index of candidate value is (1+ (* 3 CANDIDATE-INDEX)).

This function also ensures that candidate value can overwrite other parts:
Increase CANDIDATE-PLANNED-LENGTH if needs to.
|<- planned ->|
cannd          part1          part2
cannndddddddd  part1          part2
if not enough, increase to:
|<-- new planned-->|
cannnnnnnnnnnnnnd             part2"
  (let* ((candidate-real-index (1+ (* 3 candidate-index)))
         (index-after-candidate (1+ candidate-real-index))
         (candidate (nth candidate-real-index seq))
         (candidate-pad-length (length (nth index-after-candidate seq)))
         (candidate-len (length candidate))
         ;; later we will delete some element of seq
         (seq (copy-tree seq)))
    ;; 1. make sure candidate has enought space
    (when (equal 0 candidate-pad-length)
      (while (> candidate-len candidate-planned-length)
        ;; give the space of the element right after candidate to candidate
        (setq candidate-planned-length
              (+ candidate-planned-length
                 (length (nth index-after-candidate seq))))
        (setq seq (ifrich--delete-nth index-after-candidate seq)))
    ;; 2. concat everything together
    ;; 2.1 pad candidate to have length of candidate-planned-length
      (ifrich--set-nth candidate-real-index seq
                       (concat candidate
                               (make-string
                                (ifrich--zero-if-negative (- candidate-planned-length candidate-len))
                                ifrich-padding))))
    ;; 2.2 concat everything
    (apply 'concat seq)))

;;
;;;; Helper functions

(defun ifrich--zero-if-negative (num)
  "If NUM < 0, return 0, else return NUM."
  (if (< num 0)
      0
    num))

(defun ifrich--delete-nth (index seq)
  "Delete the INDEX th element of SEQ."
  (setcdr (nthcdr (1- index) seq) (nthcdr (1+ index) seq))
  seq)

(defun ifrich--set-nth (index seq newval)
  "Set the INDEX th element of SEQ to NEWVAL."
  (setcar (nthcdr index seq) newval))

;; test code
;; (let* ((format-element '((value . (lambda (str) "hahaha"))))
;;        (candidate "cand")
;;        (info-list ())
;;        (func (alist-get 'value format-element)))
;;   (setf (alist-get 'value format-element) (funcall func candidate))
;;   (add-to-list 'info-list format-element)
;;   info-list)

(defun ifrich--calculate-entry-length (seq)
  "Calculate the length of the candidate SEQ (a list of string) (one line in ivy buffer)."
  (apply 'concat seq))

(defun ifrich--is-candidate (info)
  "Check if the INFO element represents candidate."
  (if (alist-get 'candidate info)
      t
    nil))



(provide 'ivy-filthy-rich)

;;; ivy-filthy-rich.el ends here
