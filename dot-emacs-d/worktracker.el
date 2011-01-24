;; Simple interactive work tracker similar to timeclock.el.
;;
;; Chris Done 2009, Public domain
;;
;; Suggested setup:
;; ;; Work tracker
;; (load "worktracker.el")
;; ;;; Clock into project
;; (global-set-key [f9] 'work-clockin)
;; ;;; Clock out of project (done)
;; (global-set-key [f10] 'work-clockout)
;; ;;; Pause project (interrupted)
;; (global-set-key [f12] 'work-interrupted) ;; f12 is hardest to mispress
;; ;;; Resume project
;; (global-set-key [f11] 'work-resume)

;; Configuration
;;; Projects
(defvar *worktracker-projects* '("Northwood Bonbles" "Barnes and Needles"))
;; or
;; (load "jobs.el")

;;; Common reasons for clocking out
(defvar *worktracker-clockout-reasons*
  '("Done" "Interrupted" "Break" "Home" "Leaving" "Waiting"))

;;; Common reasons for clocking in
(defvar *worktracker-worktypes*
  '("Dev" "Fix" "NonFunc" "Chase"))

;;; Common causes for interruptions (i.e. people)
(defvar *worktracker-interrupters*
  '("Michael" "James" "Jannet" "Dave"))

;;; Log file location
(defvar *worktracker-log-file* "~/.worktracker.log")

;;; Globals; don't edit
(defvar *worktracker-date-time-format* "%Y/%m/%d %H:%M:%S")
(defvar *worktracker-currentproject* nil) ;; String is stored in here.
(defvar *worktracker-lastproject* nil) ;; String is stored in here.

;; Top-level interactive functions
(defun work-clockin (&optional project reason.)
  "Clock into a project, will prompt for one if not given."
  (interactive)
  (if project
      (check-and-maybe-clockin project reason.)
    (let ((project
           (ido-completing-read
            "Which project? "
            *worktracker-projects*))
          (reason (or reason.
                      (ido-completing-read
                       "What will you be working on? "
                       *worktracker-worktypes*))))
      (check-and-maybe-clockin project reason))))

(defun work-clockout (&optional reason)
  "Clock out of a project, will prompt for a reason if not given."
  (interactive)
  (maybe-clockout reason))

(defun work-interrupted (&optional cause.)
  "Clock out of a project as [Interrupted,<cause>]."
  (interactive)
  (let ((cause (or cause.
                   (ido-completing-read "Interrupted by... " 
                                        *worktracker-interrupters*))))
    (work-clockout (concat "Interrupted," cause))))

(defun work-resume ()
  "Resume the recently clocked out project."
  (lambda ()
    (interactive)
    (work-clockin *worktracker-lastproject* *worktracker-lasttype*)))

;; Helper functions
(defun check-and-maybe-clockin (project reason)
  "Check if we're in a project, if so, clock that out, then clock in."
  (if *worktracker-currentproject*
      (let ((question
             (concat "You are already clocked into `"
                     *worktracker-currentproject*
                     "', clock out? ")))
        (if (y-or-n-p question)
            (progn (actually-clockout "Leaving")
                   (actually-clockin project reason))
          (message "Nevermind, then.")))
    (actually-clockin project reason)))

(defun actually-clockin (project &optional reason)
  "Actually clock into a project."
  (setq *worktracker-currentproject* project)
  (setq *worktracker-lastproject* project)
  (setq *worktracker-lasttype* reason)
  (clock 'in project reason)
  (message (concat "Clocked into project `" project "'.")))

(defun maybe-clockout (&optional reason)
  "Clock out of the project if we're clocked into anything."
  (if *worktracker-currentproject*
      (actually-clockout reason)
    (message "Not working on anything.")))

(defun actually-clockout (&optional reason.)
  "Actually clock out of a project."
  (let ((reason (or reason.
                    (ido-completing-read "Why clockout? "
                                         *worktracker-clockout-reasons*))))
    (clock 'out *worktracker-currentproject* reason)
    (message (concat "Clocked out of project `"
                     *worktracker-currentproject*
                     "' [" reason "]."))
    (setq *worktracker-currentproject* nil)))

(defun clock (direction project reason)
  "Log a date/time project and optional reason to the log file."
  (let ((buffer (current-buffer)))
    (find-file *worktracker-log-file*)
    (goto-char (point-max))
    (insert (concat (format-time-string *worktracker-date-time-format*
                                        (current-time))
                    (if (eq direction 'in) " <- " " -> ")
                    project
                    (if reason (concat " [" reason "]") "")
                    "\n"))
    (save-buffer)
    (switch-to-buffer buffer)))
