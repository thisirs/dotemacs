;;; org-google-weather.el --- Show Google Weather forecasts in Org agenda.

;; Copyright (C) 2010 Julien Danjou

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This module allows to display the weather forecast fetched from Google in
;; your Org agenda.
;;
;;     Wednesday   8 September 2010
;;       Weather:    Pluie, 12/18 ℃
;;     Thursday    9 September 2010
;;       Weather:    Couverture nuageuse partielle, 11/21 ℃
;;
;; Just add the following in an Org buffer:
;; %%(org-google-weather)
;;
;;; Code:

(require 'google-weather)
(require 'image)
(require 'format-spec)
(require 'solar)

(defgroup org-google-weather nil
  "Google Weather for Org mode."
  :group 'comm
  :group 'org)

(defcustom org-google-weather-location calendar-location-name
  "Default location for org-google-weather."
  :group 'org-google-weather)

(defcustom org-google-weather-format "%i %c, [%l,%h] %s"
  "String to return to describe the weather.
Valid %-sequences are:
  - %i the icon
  - %c means the weather condition
  - %L the supplied location
  - %C the city the weather is for
  - %l the lower temperature
  - %h the higher temperature
  - %s the temperature unit symbol")

(defcustom org-google-weather-cache-time 43200
  "Define for how many seconds we should cache the weather."
  :group 'org-google-weather)

(defcustom org-google-weather-display-icon-p t
  "Display icons."
  :group 'org-google-weather)

(defcustom org-google-weather-icon-directory "/usr/share/icons/gnome/16x16/status"
  "Directory where to find icon listed in `org-google-weather-icon-alist'."
  :group 'org-google-weather)

(defcustom org-google-weather-icon-alist
  '((chance_of_rain . "weather-showers-scattered.png")
    (chance_of_snow . "weather-snow.png")
    (chance_of_storm . "weather-storm.png")
    (cn_cloudy . "weather-overcast.png")
    (cn_heavyrun . "weather-showers.png")
    (cn_sunny . "weather-clear.png")
    (cloudy . "weather-overcast.png")
    (dust . "weather-fog.png")
    (flurries . "weather-storm.png")
    (fog . "weather-fog.png")
    (haze . "weather-fog.png")
    (icy . "weather-snow.png")
    (jp_sunny . "weather-clear.png")
    (jp_cloudy . "weather-overcast.png")
    (mist . "weather-storm.png")
    (mostly_cloudy . "weather-overcast.png")
    (mostly_sunny . "weather-clear.png")
    (partly_cloudy . "weather-few-clouds.png")
    (rain . "weather-showers.png")
    (rain_snow . "weather-snow.png")
    (sleet . "weather-snow.png")
    (smoke . "weather-fog.png")
    (snow . "weather-snow.png")
    (storm . "weather-storm.png")
    (thunderstorm . "weather-storm.png")
    (sunny . "weather-clear.png"))
  "Icons to use to illustrate the weather."
  :group 'org-google-weather)

(defcustom org-google-weather-use-google-icons nil
  "Fetch icons from Google or use local ones.
If you decide to use local ones, you should check
`org-google-weather-icon-directory' and
`org-google-weather-icon-alist'. Otherwise, if you want to use
icons from Google, you have nothing to do."
  :group 'org-google-weather
  :type 'boolean)

(defun org-google-weather-get-icon (url)
  (with-current-buffer
      (google-weather-retrieve-data-raw url org-google-weather-cache-time)
    (goto-char (point-min))
    (unless (search-forward "\n\n" nil t)
      (error "Data not found"))
    (let ((data (buffer-substring (point) (point-max))))
      (kill-buffer (current-buffer))
      data)))

;;;###autoload
(defun org-google-weather (&optional location language)
  "Return Org entry with the weather for LOCATION in LANGUAGE.
If LOCATION is not set, use org-google-weather-location."
  (let* ((location (or location org-google-weather-location))
         (data (ignore-errors
                 (google-weather-get-data location
                                          language
                                          org-google-weather-cache-time)))
         (problem-cause (when data (google-weather-data->problem-cause data)))
         (forecast (when (and (null problem-cause) data)
                     (google-weather-data->forecast-for-date data date))))
    (if problem-cause
        (message "%s: %s" location problem-cause)
      (when forecast
        (let ((condition (cadr (assoc 'condition forecast)))
              (low (cadr (assoc 'low forecast)))
              (high (cadr (assoc 'high forecast)))
              (city (google-weather-data->city data))
              ;; But *they* told me it's just about calling functions!
              (icon (when (and org-google-weather-display-icon-p (display-images-p))
                      (if org-google-weather-use-google-icons
                          (create-image (org-google-weather-get-icon
                                         (cadr (assoc 'icon forecast)))
                                        nil t)
                        (create-image
                         (concat
                          org-google-weather-icon-directory
                          "/"
                          (cdr
                           (assoc
                            (intern
                             (file-name-sans-extension
                              (file-name-nondirectory
                               (cadr (assoc 'icon forecast)))))
                            org-google-weather-icon-alist)))))))
              (temp-symbol (google-weather-data->temperature-symbol data)))
          (format-spec org-google-weather-format
                       `((?i . ,(if icon
                                    (propertize "icon"
                                                'display
                                                (append
                                                 icon '(:ascent center))
                                                'rear-nonsticky '(display))
                                  ""))
                         (?c . ,condition)
                         (?L . ,location)
                         (?C . ,city)
                         (?l . ,low)
                         (?h . ,high)
                         (?s . ,temp-symbol))))))))

(provide 'org-google-weather)
