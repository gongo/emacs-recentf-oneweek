;;; recentf-oneweek.el --- 私、1週間でよく開いたファイルの記憶を失くしてしまうの…

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;; URL: https://github.com/gongo/emacs-recentf-oneweek
;; Keywords: recentf

;; Copyright (c) 2012 Wataru MIYAGUNI
;;
;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; (recentf-oneweek:enable) の瞬間、先週まで開いたファイルで
;; とくにアクセス数が多いものを recentf から消去する

;; Use the command `json-reformat-region'.

;;; Code:

(require 'recentf)

(eval-when-compile (require 'cl))

(defvar recentf-oneweek->start-week 1
  "一週間の記憶がリセットされる曜日。デフォルトでは月曜日")
(defvar recentf-oneweek->memories nil
  "これまでアクセスしたファイルとその回数。多ければ多いほど消える")
(defvar recentf-oneweek->last-memories-time nil
  "最後に1週間モードが起動された時間")
(defvar recentf-oneweek->save-file
  (convert-standard-filename (expand-file-name "~/.recentf_oneweek")))

(defadvice recentf-track-opened-file (after recentf-oneweek-count disable)
  (let ((filename buffer-file-name))
    (when filename
      (setq filename (recentf-expand-file-name filename))
      (when (recentf-include-p filename)
        (recentf-oneweek:push filename)))))

(defun recentf-oneweek:push (filename)
  (let ((current (or (cdr (assoc filename recentf-oneweek->memories)) 0)))
    (setq recentf-oneweek->memories (delete filename recentf-oneweek->memories))
    (add-to-list 'recentf-oneweek->memories (cons filename (1+ current)))))

(defun recentf-oneweek:load-memories ()
  (let ((file (expand-file-name recentf-oneweek->save-file)))
    (when (file-readable-p file)
      (load-file file))))

(defun recentf-oneweek:clear-memories ()
  "閾値を越えるアクセス数を持つファイルを、recentf から消去する"
  (let ((threshold (recentf-oneweek:threshold)))
    (when threshold
      (dolist (x recentf-oneweek->memories)
        (when (> (cdr x) threshold)
          (setq recentf-list (delete (car x) recentf-list))))
      (setq recentf-oneweek->memories nil)
      (recentf-save-list)
      (recentf-oneweek:save-memories))))

(defun recentf-oneweek:next-week-p ()
  (let* ((current-day  (time-to-days (current-time)))
         (current-week (recentf-oneweek:current-day-of-week))
         (prev-day     (time-to-days recentf-oneweek->last-memories-time))
         (prev-week    (- current-week (- current-day prev-day))))
    (< prev-week 0)))

(defun recentf-oneweek:threshold ()
  "閾値はアクセス数の平均値"
  (let ((len (length recentf-oneweek->memories))
        (sum 0))
    (when (> len 0)
      (dolist (var recentf-oneweek->memories)
        (setq sum (+ sum (cdr var))))
      (/ sum len))))

(defun recentf-oneweek:current-day-of-week ()
  (let (week)
    (setq week (calendar-day-of-week (calendar-current-date)))
    (mod (+ (- week recentf-oneweek->start-week) 7) 7)))

(defun recentf-oneweek:dump-variable (variable)
  (insert (format "\n(setq %S '%S)\n" variable (symbol-value variable))))

(defun recentf-oneweek:save-memories ()
  "referring to `recentf-save-list'"
  (interactive)
  (setq recentf-oneweek->last-memories-time (current-time))
  (with-temp-buffer
    (erase-buffer)
    (set-buffer-file-coding-system recentf-save-file-coding-system)
    (recentf-oneweek:dump-variable 'recentf-oneweek->memories)
    (recentf-oneweek:dump-variable 'recentf-oneweek->last-memories-time)
    (insert "\n\n;; Local Variables:\n"
            (format ";; coding: %s\n" recentf-save-file-coding-system)
            ";; End:\n")
    (write-file (expand-file-name recentf-oneweek->save-file))))

(defun recentf-oneweek:enable ()
  "1週間モード起動時が前回起動した時の翌週と判断された時、記憶は消去される"
  (recentf-mode 1)
  (recentf-oneweek:load-memories)

  (when (recentf-oneweek:next-week-p)
    (recentf-oneweek:clear-memories))

  (ad-enable-advice 'recentf-track-opened-file 'after 'recentf-oneweek-count)
  (ad-activate 'recentf-track-opened-file)
  (add-hook 'kill-emacs-hook 'recentf-oneweek:save-memories))

(defun recentf-oneweek:disable ()
  (recentf-oneweek:save-memories)
  (ad-disable-advice 'recentf-track-opened-file 'after 'recentf-oneweek-count)
  (ad-activate 'recentf-track-opened-file)
  (remove-hook 'kill-emacs-hook 'recentf-oneweek:save-memories))

(provide 'recentf-oneweek)
