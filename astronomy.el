;;; astronomy.el --- compute astronomical, nautical and civil twilight

;; Copyright (C) 2001 Bill White 

;; Author: Bill White <billw@wolfram.com>
;; Keywords: calendar
;; Human-Keywords: calendar, solar, twilight, astronomical, nautical, 
;; Human-Keywords: civil, astronomy
;; Version: 0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

;; Please send suggestions and bug reports to <billw@wolfram.com>.
;; The latest version of this package should be available from
;;
;;     <url:http://members.wri.com/billw/astronomy>

;;; Commentary:

;; OVERVIEW ==========================================================

;; This is a quick and dirty hack of Edward M. Reingold's solar code
;; (part of his calendar package) to compute the times of
;; astronomical, nautical and civil twilight in the morning and
;; evening.  It was motivated by a note in the March 2001 newsletter
;; of the U.S. National Council of Catholic Bishops' Committee on the
;; Liturgy:

;; ,----[ <url:http://www.nccbuscc.org/liturgy/innews/032001.htm> ]
;; | The intention of the <Missale Romanum> is clear: the Easter Vigil is
;; | to take place in darkness. Thus the approved translation of <post
;; | initium noctis> is after nightfall, that is, after the time in the
;; | evening when daylight is last visible. This time is roughly
;; | equivalent to astronomical twilight, which is defined by the Naval
;; | Observatory as the time after which "the Sun does not contribute to
;; | sky illumination." Tables of sunset and astronomical twilight for
;; | each locality in the United States are available at
;; | http://aa.usno.navy.mil/data/docs/RS_OneYear.html.
;; | 
;; | In Washington, DC, by way of example, sunset will take place at
;; | 6:45pm on Holy Saturday, April 15, 2001. However, Astronomical
;; | Twilight in the nation's capital will not occur until 8:21pm, or 96
;; | minutes later.  Likewise, sunset in Los Angeles occurs at 6:25pm,
;; | but Astronomical Twilight (when "the Sun does not contribute to sky
;; | illumination") occurs at 7:53 pm, about 88 minutes later. While
;; | some pastoral flexibility concerning the astronomical mathematics
;; | of the question is reasonable, it is clearly the intent of the
;; | Church that the Easter Vigil not begin until it is dark.
;; `----

;; See <url:http://aa.usno.navy.mil/faq/docs/RST_defs.html> for
;; definitions of astronomical, nautical and civil twilight.

;; I've named this thing "astronomy.el" with the intention of adding
;; more astronomical functions to it.  Next up are solar noon (which I
;; think Dr. Reingold has already done) and daily lunar times, times
;; and azimuths for rising and setting objects (which EMR has already
;; done for the Sun), then whatever astronomical information seems
;; interesting, useful or fun.

;; To paraphrase an old saying, I didn't have time to write a short
;; package so I wrote a long one.

;; INSTALLATION ======================================================

;; You should have these lines somewhere in your startup file(s):

;; (require 'astronomy)
;; (add-hook 'diary-display-hook 'fancy-diary-display)

;; Put astronomy.el somewhere in your emacs load path (C-h v load-path
;; <ret>), byte-compile it, and put the following lines in your
;; 'diary-file' (see 'C-h v diary-file <ret>'):

;; &%%(diary-morning-astronomical-twilight)
;; &%%(diary-morning-nautical-twilight)
;; &%%(diary-morning-civil-twilight)
;; &%%(diary-sunrise)
;; &%%(diary-sunset)
;; &%%(diary-evening-civil-twilight)
;; &%%(diary-evening-nautical-twilight)
;; &%%(diary-evening-astronomical-twilight)

; ; Invoke the calendar (M-x calendar) and type 'd'. 

;; ACKOWLEDGEMENTS ===================================================

;; - Edward M. Reingold for the calendar package and encouragement,
;; - Kevin Rodgers <kevinr@ihs.com> for testing and output formatting ideas,
;; - Steve Peter <speter@wolfram.com> for testing

;; HISTORY ===========================================================

;; 0.2: added diary-sunrise and diary-sunset, reformatted the output to
;; report times first.

;; 0.1: initial release

;;; Code:

(require 'calendar)
(require 'solar)

;; (defun diary-sunrise-sunset ()
;;   "Local time of sunrise and sunset as a diary entry.
;; Accurate to a few seconds."
;;   (if (not (and calendar-latitude calendar-longitude calendar-time-zone))
;;       (solar-setup))
;;   (solar-sunrise-sunset-string date))

(defun diary-sunrise ()
  "Local time of sunrise as a diary entry.
Accurate to a few seconds."
  (if (not (and calendar-latitude calendar-longitude calendar-time-zone))
      (solar-setup))
  (solar-sunrise-string date))

(defun diary-sunset ()
  "Local time of sunset as a diary entry.
Accurate to a few seconds."
  (if (not (and calendar-latitude calendar-longitude calendar-time-zone))
      (solar-setup))
  (solar-sunset-string date))

;; (defun solar-sunrise-sunset-string (date)
;;   "String of *local* times of sunrise, sunset, and daylight on Gregorian DATE."
;;   (let ((l (solar-sunrise-sunset date)))
;;     (format
;;      "%s, %s at %s (%s hours daylight)"
;;      (if (car l)
;;          (concat "Sunrise " (apply 'solar-time-string (car l)))
;;        "No sunrise")
;;      (if (car (cdr l))
;;          (concat "sunset " (apply 'solar-time-string (car (cdr l))))
;;        "no sunset")
;;      (eval calendar-location-name)
;;      (car (cdr (cdr l))))))

(defun solar-sunrise-string (date)
  "String of *local* time of sunrise on Gregorian DATE."
  (let ((l (solar-sunrise-sunset date)))
    (format
     "%s at %s"
     (if (car l)
	 (concat (apply 'solar-time-string (car l)) ": Sunrise")
       "No sunrise")
     (eval calendar-location-name))))

(defun solar-sunset-string (date)
  "String of *local* time of sunset and daylight on Gregorian DATE."
  (let ((l (solar-sunrise-sunset date)))
    (format
     "%s at %s (%s hours daylight)"
     (if (car (cdr l))
         (concat (apply 'solar-time-string (car (cdr l))) ": Sunset")
       "No sunset")
     (eval calendar-location-name)
     (car (cdr (cdr l))))))

;----------------------------------------------------------------------

(defun diary-morning-astronomical-twilight ()
  "Local time of morning astronomical twilight as a diary entry.
Accurate to a few seconds."
  (if (not (and calendar-latitude calendar-longitude calendar-time-zone))
      (solar-setup))
  (solar-morning-astronomical-twilight-string date))

(defun diary-morning-nautical-twilight ()
  "Local time of morning nautical twilight as a diary entry.
Accurate to a few seconds."
  (if (not (and calendar-latitude calendar-longitude calendar-time-zone))
      (solar-setup))
  (solar-morning-nautical-twilight-string date))

(defun diary-morning-civil-twilight ()
  "Local time of morning civil twilight as a diary entry.
Accurate to a few seconds."
  (if (not (and calendar-latitude calendar-longitude calendar-time-zone))
      (solar-setup))
  (solar-morning-civil-twilight-string date))

(defun diary-evening-astronomical-twilight ()
  "Local time of evening astronomical twilight as a diary entry.
Accurate to a few seconds."
  (if (not (and calendar-latitude calendar-longitude calendar-time-zone))
      (solar-setup))
  (solar-evening-astronomical-twilight-string date))

(defun diary-evening-nautical-twilight ()
  "Local time of evening nautical twilight as a diary entry.
Accurate to a few seconds."
  (if (not (and calendar-latitude calendar-longitude calendar-time-zone))
      (solar-setup))
  (solar-evening-nautical-twilight-string date))

(defun diary-evening-civil-twilight ()
  "Local time of evening civil twilight as a diary entry.
Accurate to a few seconds."
  (if (not (and calendar-latitude calendar-longitude calendar-time-zone))
      (solar-setup))
  (solar-evening-civil-twilight-string date))

;; (defun solar-sunrise-sunset-string (date)
;;   "String of *local* times of sunrise, sunset, and daylight on Gregorian DATE."
;;   (let ((l (solar-sunrise-sunset date)))
;;     (format
;;      "%s, %s at %s (%s hours daylight)"
;;      (if (car l)
;;          (concat "Sunrise " (apply 'solar-time-string (car l)))
;;        "No sunrise")
;;      (if (car (cdr l))
;;          (concat "sunset " (apply 'solar-time-string (car (cdr l))))
;;        "no sunset")
;;      (eval calendar-location-name)
;;      (car (cdr (cdr l))))))

(defun solar-morning-astronomical-twilight-string (date)
  "String of *local* time of morning astronomical twilight, and
daylight on Gregorian DATE."
  (let ((l (solar-astronomical-twilight date)))
    (format
     "%s at %s"
     (if (car l)
         (concat (apply 'solar-time-string (car l)) ": Astronomical twilight begins")
       "No astronomical twilight")
     (eval calendar-location-name)
)))

(defun solar-morning-nautical-twilight-string (date)
  "String of *local* time of morning nautical twilight, and
daylight on Gregorian DATE."
  (let ((l (solar-nautical-twilight date)))
    (format
     "%s at %s"
     (if (car l)
         (concat (apply 'solar-time-string (car l)) ": Nautical twilight begins")
       "No nautical twilight")
     (eval calendar-location-name))))

(defun solar-morning-civil-twilight-string (date)
  "String of *local* time of morning civil twilight, and
daylight on Gregorian DATE."
  (let ((l (solar-civil-twilight date)))
    (format
     "%s at %s"
     (if (car l)
         (concat (apply 'solar-time-string (car l)) ": Civil twilight begins")
       "No civil twilight")
     (eval calendar-location-name))))

(defun solar-evening-astronomical-twilight-string (date)
  "String of *local* time of evening astronomical twilight, and
daylight on Gregorian DATE."
  (let ((l (solar-astronomical-twilight date)))
    (format
     "%s at %s"
     (if (car (cdr l))
         (concat (apply 'solar-time-string (car (cdr l))) ": Astronomical twilight ends")
       "No astronomical twilight")
     (eval calendar-location-name))))

(defun solar-evening-nautical-twilight-string (date)
  "String of *local* time of evening nautical twilight, and
daylight on Gregorian DATE."
  (let ((l (solar-nautical-twilight date)))
    (format
     "%s at %s"
     (if (car (cdr l))
         (concat (apply 'solar-time-string (car (cdr l))) ": Nautical twilight ends")
       "No nautical twilight")
     (eval calendar-location-name))))

(defun solar-evening-civil-twilight-string (date)
  "String of *local* time of evening civil twilight, and
daylight on Gregorian DATE."
  (let ((l (solar-civil-twilight date)))
    (format
     "%s at %s"
     (if (car (cdr l))
         (concat (apply 'solar-time-string (car (cdr l))) ": Civil twilight ends")
       "No civil twilight")
     (eval calendar-location-name))))

;; (defun solar-sunrise-sunset (date)
;;   "List of *local* times of sunrise, sunset, and daylight on Gregorian DATE.

;; Corresponding value is nil if there is no sunrise/sunset."
;;   (let* (; first, get the exact moment of local noon.
;;          (exact-local-noon (solar-exact-local-noon date))
;;          ; get the the time from the 2000 epoch.
;;          (t0 (solar-julian-ut-centuries (car exact-local-noon)))
;;          ; store the sidereal time at Greenwich at midnight of UT time.
;;          ; find if summer or winter slightly above the equator
;;          (equator-rise-set
;;           (progn (setq solar-sidereal-time-greenwich-midnight 
;;                        (solar-sidereal-time t0))
;;                  (solar-sunrise-and-sunset 
;;                   (list t0 (car (cdr exact-local-noon)))
;;                   10.0
;;                   (calendar-longitude))))
;;          ; store the spring/summer information,
;;          ; compute sunrise and sunset (two first components of rise-set).
;;          ; length of day is the third component (it is only the difference
;;          ; between sunset and sunrise when there is a sunset and a sunrise)
;;          (rise-set
;;           (progn
;;             (setq solar-spring-or-summer-season 
;;                   (if (> (car (cdr (cdr equator-rise-set))) 12) 1 0))
;;             (solar-sunrise-and-sunset 
;;              (list t0 (car (cdr exact-local-noon)))
;;              (calendar-latitude)
;;              (calendar-longitude))))
;;          (rise (car rise-set))
;;          (adj-rise (if rise (dst-adjust-time date rise) nil))
;;          (set (car (cdr rise-set)))
;;          (adj-set (if set (dst-adjust-time date set) nil))
;;          (length  (car (cdr (cdr rise-set)))) )
;;     (list
;;      (and rise (calendar-date-equal date (car adj-rise)) (cdr adj-rise))
;;      (and set (calendar-date-equal date (car adj-set)) (cdr adj-set))
;;      (solar-daylight length))))

(defun solar-astronomical-twilight (date)
  "List of *local* times of morning and evening astronomical twilight,
and daylight on Gregorian DATE.

Corresponding value is nil if there is no sunrise/sunset."
  (let* (; first, get the exact moment of local noon.
         (exact-local-noon (solar-exact-local-noon date))
         ; get the the time from the 2000 epoch.
         (t0 (solar-julian-ut-centuries (car exact-local-noon)))
         ; store the sidereal time at Greenwich at midnight of UT time.
         ; find if summer or winter slightly above the equator
         (equator-rise-set
          (progn (setq solar-sidereal-time-greenwich-midnight 
                       (solar-sidereal-time t0))
                 (solar-astronomical-twilight-main 
                  (list t0 (car (cdr exact-local-noon)))
                  10.0
                  (calendar-longitude))))
         ; store the spring/summer information,
         ; compute sunrise and sunset (two first components of rise-set).
         ; length of day is the third component (it is only the difference
         ; between sunset and sunrise when there is a sunset and a sunrise)
         (rise-set
          (progn
            (setq solar-spring-or-summer-season 
                  (if (> (car (cdr (cdr equator-rise-set))) 12) 1 0))
            (solar-astronomical-twilight-main 
             (list t0 (car (cdr exact-local-noon)))
             (calendar-latitude)
             (calendar-longitude))))
         (rise (car rise-set))
         (adj-rise (if rise (dst-adjust-time date rise) nil))
         (set (car (cdr rise-set)))
         (adj-set (if set (dst-adjust-time date set) nil))
         (length  (car (cdr (cdr rise-set)))) )
    (list
     (and rise (calendar-date-equal date (car adj-rise)) (cdr adj-rise))
     (and set (calendar-date-equal date (car adj-set)) (cdr adj-set))
     (solar-daylight length))))

(defun solar-nautical-twilight (date)
  "List of *local* times of morning and evening nautical twilight,
and daylight on Gregorian DATE.

Corresponding value is nil if there is no sunrise/sunset."
  (let* (; first, get the exact moment of local noon.
         (exact-local-noon (solar-exact-local-noon date))
         ; get the the time from the 2000 epoch.
         (t0 (solar-julian-ut-centuries (car exact-local-noon)))
         ; store the sidereal time at Greenwich at midnight of UT time.
         ; find if summer or winter slightly above the equator
         (equator-rise-set
          (progn (setq solar-sidereal-time-greenwich-midnight 
                       (solar-sidereal-time t0))
                 (solar-nautical-twilight-main 
                  (list t0 (car (cdr exact-local-noon)))
                  10.0
                  (calendar-longitude))))
         ; store the spring/summer information,
         ; compute sunrise and sunset (two first components of rise-set).
         ; length of day is the third component (it is only the difference
         ; between sunset and sunrise when there is a sunset and a sunrise)
         (rise-set
          (progn
            (setq solar-spring-or-summer-season 
                  (if (> (car (cdr (cdr equator-rise-set))) 12) 1 0))
            (solar-nautical-twilight-main 
             (list t0 (car (cdr exact-local-noon)))
             (calendar-latitude)
             (calendar-longitude))))
         (rise (car rise-set))
         (adj-rise (if rise (dst-adjust-time date rise) nil))
         (set (car (cdr rise-set)))
         (adj-set (if set (dst-adjust-time date set) nil))
         (length  (car (cdr (cdr rise-set)))) )
    (list
     (and rise (calendar-date-equal date (car adj-rise)) (cdr adj-rise))
     (and set (calendar-date-equal date (car adj-set)) (cdr adj-set))
     (solar-daylight length))))

(defun solar-civil-twilight (date)
  "List of *local* times of morning and evening civil twilight,
and daylight on Gregorian DATE.

Corresponding value is nil if there is no sunrise/sunset."
  (let* (; first, get the exact moment of local noon.
         (exact-local-noon (solar-exact-local-noon date))
         ; get the the time from the 2000 epoch.
         (t0 (solar-julian-ut-centuries (car exact-local-noon)))
         ; store the sidereal time at Greenwich at midnight of UT time.
         ; find if summer or winter slightly above the equator
         (equator-rise-set
          (progn (setq solar-sidereal-time-greenwich-midnight 
                       (solar-sidereal-time t0))
                 (solar-civil-twilight-main 
                  (list t0 (car (cdr exact-local-noon)))
                  10.0
                  (calendar-longitude))))
         ; store the spring/summer information,
         ; compute sunrise and sunset (two first components of rise-set).
         ; length of day is the third component (it is only the difference
         ; between sunset and sunrise when there is a sunset and a sunrise)
         (rise-set
          (progn
            (setq solar-spring-or-summer-season 
                  (if (> (car (cdr (cdr equator-rise-set))) 12) 1 0))
            (solar-civil-twilight-main 
             (list t0 (car (cdr exact-local-noon)))
             (calendar-latitude)
             (calendar-longitude))))
         (rise (car rise-set))
         (adj-rise (if rise (dst-adjust-time date rise) nil))
         (set (car (cdr rise-set)))
         (adj-set (if set (dst-adjust-time date set) nil))
         (length  (car (cdr (cdr rise-set)))) )
    (list
     (and rise (calendar-date-equal date (car adj-rise)) (cdr adj-rise))
     (and set (calendar-date-equal date (car adj-set)) (cdr adj-set))
     (solar-daylight length))))

;; (defun solar-sunrise-and-sunset (time latitude longitude)
;;   "Sunrise, sunset and length of day. 
;; Parameters are the midday TIME and the LATITUDE, LONGITUDE of the location.

;; TIME is a pair with the first component being the number of Julian centuries
;; elapsed at 0 Universal Time, and the second component being the universal
;; time.  For instance, the pair corresponding to November 28, 1995 at 16 UT is
;; \(-0.040945 16), -0.040945 being the number of julian centuries elapsed between
;; Jan 1, 2000 at 12 UT and November 28, 1995 at 0 UT.

;; Coordinates are included because this function is called with latitude=10
;; degrees to find out if polar regions have 24 hours of sun or only night."
;;   (let* ((rise-time (solar-moment -1 latitude longitude time))
;;          (set-time (solar-moment 1 latitude longitude time))
;;          (day-length))
;;     (if (not (and rise-time set-time))
;;         (if (or (and (> latitude 0) solar-spring-or-summer-season)
;;                 (and (< latitude 0) (not solar-spring-or-summer-season)))
;;           (setq day-length 24)
;;           (setq day-length 0))
;;         (setq day-length (- set-time rise-time)))
;;     (list (if rise-time (+ rise-time (/ calendar-time-zone 60.0)) nil)
;;           (if set-time (+ set-time (/ calendar-time-zone 60.0)) nil)
;;           day-length)))

(defun solar-astronomical-twilight-main (time latitude longitude)
  "Morning and evening astronomical twilight and length of day. 
Parameters are the midday TIME and the LATITUDE, LONGITUDE of the location.

TIME is a pair with the first component being the number of Julian centuries
elapsed at 0 Universal Time, and the second component being the universal
time.  For instance, the pair corresponding to November 28, 1995 at 16 UT is
\(-0.040945 16), -0.040945 being the number of julian centuries elapsed between
Jan 1, 2000 at 12 UT and November 28, 1995 at 0 UT.

Coordinates are included because this function is called with latitude=10
degrees to find out if polar regions have 24 hours of sun or only night."
  (let* ((rise-time (solar-astronomical-twilight-moment -1 latitude longitude time))
         (set-time (solar-astronomical-twilight-moment 1 latitude longitude time))
         (day-length))
    (if (not (and rise-time set-time))
        (if (or (and (> latitude 0) solar-spring-or-summer-season)
                (and (< latitude 0) (not solar-spring-or-summer-season)))
          (setq day-length 24)
          (setq day-length 0))
        (setq day-length (- set-time rise-time)))
    (list (if rise-time (+ rise-time (/ calendar-time-zone 60.0)) nil)
          (if set-time (+ set-time (/ calendar-time-zone 60.0)) nil)
          day-length)))

(defun solar-nautical-twilight-main (time latitude longitude)
  "Morning and evening nautical twilight and length of day. 
Parameters are the midday TIME and the LATITUDE, LONGITUDE of the location.

TIME is a pair with the first component being the number of Julian centuries
elapsed at 0 Universal Time, and the second component being the universal
time.  For instance, the pair corresponding to November 28, 1995 at 16 UT is
\(-0.040945 16), -0.040945 being the number of julian centuries elapsed between
Jan 1, 2000 at 12 UT and November 28, 1995 at 0 UT.

Coordinates are included because this function is called with latitude=10
degrees to find out if polar regions have 24 hours of sun or only night."
  (let* ((rise-time (solar-nautical-twilight-moment -1 latitude longitude time))
         (set-time (solar-nautical-twilight-moment 1 latitude longitude time))
         (day-length))
    (if (not (and rise-time set-time))
        (if (or (and (> latitude 0) solar-spring-or-summer-season)
                (and (< latitude 0) (not solar-spring-or-summer-season)))
          (setq day-length 24)
          (setq day-length 0))
        (setq day-length (- set-time rise-time)))
    (list (if rise-time (+ rise-time (/ calendar-time-zone 60.0)) nil)
          (if set-time (+ set-time (/ calendar-time-zone 60.0)) nil)
          day-length)))

(defun solar-civil-twilight-main (time latitude longitude)
  "Morning and evening civil twilight and length of day. 
Parameters are the midday TIME and the LATITUDE, LONGITUDE of the location.

TIME is a pair with the first component being the number of Julian centuries
elapsed at 0 Universal Time, and the second component being the universal
time.  For instance, the pair corresponding to November 28, 1995 at 16 UT is
\(-0.040945 16), -0.040945 being the number of julian centuries elapsed between
Jan 1, 2000 at 12 UT and November 28, 1995 at 0 UT.

Coordinates are included because this function is called with latitude=10
degrees to find out if polar regions have 24 hours of sun or only night."
  (let* ((rise-time (solar-civil-twilight-moment -1 latitude longitude time))
         (set-time (solar-civil-twilight-moment 1 latitude longitude time))
         (day-length))
    (if (not (and rise-time set-time))
        (if (or (and (> latitude 0) solar-spring-or-summer-season)
                (and (< latitude 0) (not solar-spring-or-summer-season)))
          (setq day-length 24)
          (setq day-length 0))
        (setq day-length (- set-time rise-time)))
    (list (if rise-time (+ rise-time (/ calendar-time-zone 60.0)) nil)
          (if set-time (+ set-time (/ calendar-time-zone 60.0)) nil)
          day-length)))

;; (defun solar-moment (direction latitude longitude time)
;;   "Sunrise/sunset at location.
;; Sunrise if DIRECTION =-1 or sunset if =1 at LATITUDE, LONGITUDE, with midday
;; being TIME.

;; TIME is a pair with the first component being the number of Julian centuries
;; elapsed at 0 Universal Time, and the second component being the universal
;; time.  For instance, the pair corresponding to November 28, 1995 at 16 UT is
;; \(-0.040945 16), -0.040945 being the number of julian centuries elapsed between
;; Jan 1, 2000 at 12 UT and November 28, 1995 at 0 UT.

;; Uses binary search."
;;   (let* ((ut (car (cdr time)))
;;          (possible 1) ; we assume that rise or set are possible
;;          (utmin (+ ut (* direction 12.0))) 
;;          (utmax ut)    ; the time searched is between utmin and utmax
;;             ; utmin and utmax are in hours
;;          (utmoment-old 0.0)    ; rise or set approximation
;;          (utmoment 1.0) ; rise or set approximation
;;          (hut 0)         ; sun height at utmoment
;;          (t0 (car time))
;;          (hmin (car (cdr 
;;                (solar-horizontal-coordinates (list t0 utmin) 
;;                                                 latitude longitude t))))
;;          (hmax (car (cdr 
;;                (solar-horizontal-coordinates (list t0 utmax) 
;;                                                 latitude longitude t)))))
;;        ; -0.61 degrees is the height of the middle of the sun, when it rises
;;        ;   or sets.
;;      (if (< hmin -0.61) 
;;               (if (> hmax -0.61)
;;                   (while ;(< i 20) ; we perform a simple dichotomy
;;                          ; (> (abs (+ hut 0.61)) epsilon)
;;                          (>= (abs (- utmoment utmoment-old))
;;                              (/ solar-error 60))
;;                     (setq utmoment-old utmoment)
;;                     (setq utmoment (/ (+ utmin utmax) 2))
;;                     (setq hut (car (cdr 
;;                                     (solar-horizontal-coordinates 
;;                                    (list t0 utmoment) latitude longitude t))))
;;                     (if (< hut -0.61) (setq utmin utmoment))
;;                     (if (> hut -0.61) (setq utmax utmoment))
;;                    )
;;                 (setq possible 0)) ; the sun never rises
;;                 (setq possible 0)) ; the sun never sets
;;      (if (equal possible 0) nil utmoment)))

(defun solar-astronomical-twilight-moment (direction latitude longitude time)
  "Morning and evening astronomical twilight at location.
Morning if DIRECTION =-1 or evening if =1 at LATITUDE, LONGITUDE, with midday
being TIME.

TIME is a pair with the first component being the number of Julian centuries
elapsed at 0 Universal Time, and the second component being the universal
time.  For instance, the pair corresponding to November 28, 1995 at 16 UT is
\(-0.040945 16), -0.040945 being the number of julian centuries elapsed between
Jan 1, 2000 at 12 UT and November 28, 1995 at 0 UT.

Uses binary search."
  (let* ((ut (car (cdr time)))
         (possible 1) ; we assume that rise or set are possible
         (utmin (+ ut (* direction 12.0))) 
         (utmax ut)    ; the time searched is between utmin and utmax
            ; utmin and utmax are in hours
         (utmoment-old 0.0)    ; rise or set approximation
         (utmoment 1.0) ; rise or set approximation
         (hut 0)         ; sun height at utmoment
         (t0 (car time))
         (hmin (car (cdr 
               (solar-horizontal-coordinates (list t0 utmin) 
                                                latitude longitude t))))
         (hmax (car (cdr 
               (solar-horizontal-coordinates (list t0 utmax) 
                                                latitude longitude t)))))
       ; -0.61 degrees is the height of the middle of the sun, when it rises
       ;   or sets.

       ; -18.0 degrees is the height of the middle of the sun at the
       ; beginning of morning astronomical twilight and at the end of
       ; evening astronomical twilight.
     (if (< hmin -18.0) 
              (if (> hmax -18.0)
                  (while ;(< i 20) ; we perform a simple dichotomy
                         ; (> (abs (+ hut 0.61)) epsilon)
                         (>= (abs (- utmoment utmoment-old))
                             (/ solar-error 60))
                    (setq utmoment-old utmoment)
                    (setq utmoment (/ (+ utmin utmax) 2))
                    (setq hut (car (cdr 
                                    (solar-horizontal-coordinates 
                                   (list t0 utmoment) latitude longitude t))))
                    (if (< hut -18.0) (setq utmin utmoment))
                    (if (> hut -18.0) (setq utmax utmoment))
                   )
                (setq possible 0)) ; the sun never rises
                (setq possible 0)) ; the sun never sets
     (if (equal possible 0) nil utmoment)))

(defun solar-nautical-twilight-moment (direction latitude longitude time)
  "Morning and evening nautical twilight at location.
Morning if DIRECTION =-1 or evening if =1 at LATITUDE, LONGITUDE, with midday
being TIME.

TIME is a pair with the first component being the number of Julian centuries
elapsed at 0 Universal Time, and the second component being the universal
time.  For instance, the pair corresponding to November 28, 1995 at 16 UT is
\(-0.040945 16), -0.040945 being the number of julian centuries elapsed between
Jan 1, 2000 at 12 UT and November 28, 1995 at 0 UT.

Uses binary search."
  (let* ((ut (car (cdr time)))
         (possible 1) ; we assume that rise or set are possible
         (utmin (+ ut (* direction 12.0))) 
         (utmax ut)    ; the time searched is between utmin and utmax
            ; utmin and utmax are in hours
         (utmoment-old 0.0)    ; rise or set approximation
         (utmoment 1.0) ; rise or set approximation
         (hut 0)         ; sun height at utmoment
         (t0 (car time))
         (hmin (car (cdr 
               (solar-horizontal-coordinates (list t0 utmin) 
                                                latitude longitude t))))
         (hmax (car (cdr 
               (solar-horizontal-coordinates (list t0 utmax) 
                                                latitude longitude t)))))
       ; -0.61 degrees is the height of the middle of the sun, when it rises
       ;   or sets.

       ; -18.0 degrees is the height of the middle of the sun at the
       ; beginning of morning astronomical twilight and at the end of
       ; evening astronomical twilight.

       ; -12.0 degrees is the height of the middle of the sun at the
       ; beginning of morning nautical twilight and at the end of
       ; evening nautical twilight.
     (if (< hmin -12.0) 
              (if (> hmax -12.0)
                  (while ;(< i 20) ; we perform a simple dichotomy
                         ; (> (abs (+ hut 0.61)) epsilon)
                         (>= (abs (- utmoment utmoment-old))
                             (/ solar-error 60))
                    (setq utmoment-old utmoment)
                    (setq utmoment (/ (+ utmin utmax) 2))
                    (setq hut (car (cdr 
                                    (solar-horizontal-coordinates 
                                   (list t0 utmoment) latitude longitude t))))
                    (if (< hut -12.0) (setq utmin utmoment))
                    (if (> hut -12.0) (setq utmax utmoment))
                   )
                (setq possible 0)) ; the sun never rises
                (setq possible 0)) ; the sun never sets
     (if (equal possible 0) nil utmoment)))

(defun solar-civil-twilight-moment (direction latitude longitude time)
  "Morning and evening civil twilight at location.
Morning if DIRECTION =-1 or evening if =1 at LATITUDE, LONGITUDE, with midday
being TIME.

TIME is a pair with the first component being the number of Julian centuries
elapsed at 0 Universal Time, and the second component being the universal
time.  For instance, the pair corresponding to November 28, 1995 at 16 UT is
\(-0.040945 16), -0.040945 being the number of julian centuries elapsed between
Jan 1, 2000 at 12 UT and November 28, 1995 at 0 UT.

Uses binary search."
  (let* ((ut (car (cdr time)))
         (possible 1) ; we assume that rise or set are possible
         (utmin (+ ut (* direction 12.0))) 
         (utmax ut)    ; the time searched is between utmin and utmax
            ; utmin and utmax are in hours
         (utmoment-old 0.0)    ; rise or set approximation
         (utmoment 1.0) ; rise or set approximation
         (hut 0)         ; sun height at utmoment
         (t0 (car time))
         (hmin (car (cdr 
               (solar-horizontal-coordinates (list t0 utmin) 
                                                latitude longitude t))))
         (hmax (car (cdr 
               (solar-horizontal-coordinates (list t0 utmax) 
                                                latitude longitude t)))))
       ; -0.61 degrees is the height of the middle of the sun, when it rises
       ;   or sets.

       ; -18.0 degrees is the height of the middle of the sun at the
       ; beginning of morning astronomical twilight and at the end of
       ; evening astronomical twilight.

       ; -12.0 degrees is the height of the middle of the sun at the
       ; beginning of morning nautical twilight and at the end of
       ; evening nautical twilight.

       ; -6.0 degrees is the height of the middle of the sun at the
       ; beginning of morning civil twilight and at the end of
       ; evening civil twilight.
     (if (< hmin -6.0) 
              (if (> hmax -6.0)
                  (while ;(< i 20) ; we perform a simple dichotomy
                         ; (> (abs (+ hut 0.61)) epsilon)
                         (>= (abs (- utmoment utmoment-old))
                             (/ solar-error 60))
                    (setq utmoment-old utmoment)
                    (setq utmoment (/ (+ utmin utmax) 2))
                    (setq hut (car (cdr 
                                    (solar-horizontal-coordinates 
                                   (list t0 utmoment) latitude longitude t))))
                    (if (< hut -6.0) (setq utmin utmoment))
                    (if (> hut -6.0) (setq utmax utmoment))
                   )
                (setq possible 0)) ; the sun never rises
                (setq possible 0)) ; the sun never sets
     (if (equal possible 0) nil utmoment)))

(provide 'astronomy)
