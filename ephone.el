;;; ephone.el --- Leverage ofonod through Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/ephone
;; Version: 0.1
;; Package-Requires: ((emacs "?"))
;; Keywords: comm, convenience

;;; Commentary:

;; This package provides a simple interface to ofonod, the ofono daemon.
;; It is intended to be used with a mobile phone that is connected to a
;; computer via Bluetooth.
;; With this package you can send and receive phone calls, attach events. etc.

;;; Code:

;; Hooks
(setq ephone-call-hook nil)
(setq ephone-hangup-hook nil)
;;;;;;;;;;

(setq ephone--dbus-hooks nil)
(setq ephone--current-device nil)
(setq ephone--devices nil)

(defun ephone--get-bt-address (object)
  "Returns the Bluetooth address of a device. OBJECT is expected to be in the
   format of /hfp/org/bluez/hci0/dev_00_11_22_33_44_55."
  ;; The Bluetooth address is the last part of the object path
  (replace-regexp-in-string "_" ":" (replace-regexp-in-string "dev_" "" (car (last (split-string object "/"))))))

(defun ephone--get-device-name (bt-device)
  "Get the name of a Bluetooth device. BT-DEVICE is expected to be in the format of 00:11:22:33:44:55."
  (let ((device-name (dbus-get-property :system
                                        "org.bluez"
                                        (format "/org/bluez/hci0/dev_%s" (replace-regexp-in-string ":" "_" bt-device))
                                        "org.bluez.Device1"
                                        "Name")))
    (if (not device-name)
        (error "Could not get device name for %s" bt-device)
      device-name)))

(defun ephone--clean-number (number)
  "Clean up a phone number. Remove any characters except plus and numbers."
  (replace-regexp-in-string "[^0-9+]" "" number))

(defun ephone--get-hfp-devices()
  (let ((devices (dbus-introspect-get-all-nodes :system "org.ofono" "/hfp/org/bluez")))
    (when (not devices)
      (display-warning "ephone" "No Bluetooth devices found"))
    ;; Filter out anything that is not /hfp/org/bluez/hci0/dev_*
    (setq ephone--devices (seq-filter (lambda (device)
                                        (and (string-match-p "dev_" device) (not (string-match-p "voicecall" device))))
                                      devices))
    ;; Ofono/BlueZ may also cache devices that are currently not connected.
    ;; We need to filter those out.
    (setq ephone--devices (seq-filter (lambda (device)
                                        (let ((bt-device (ephone--get-bt-address device)))
                                          (dbus-get-property :system
                                                             "org.bluez"
                                                             (concat "/" (string-join (cddr (split-string device "/")) "/"))
                                                             "org.bluez.Device1"
                                                             "Connected")))
                                      ephone--devices))
    ephone--devices))

(defun ephone--call-added (path properties)
  (run-hooks 'ephone-call-hook))

(defun ephone--call-removed (path)
  (run-hooks 'ephone-hangup-hook))

(defun ephone--get-calls (&rest device)
  "Get a list of calls present on DEVICE. Or all devices if DEVICE is nil."
  (ephone--get-hfp-devices)
  (if device
      (ephone--vcm-interface-send device "GetCalls")
    (mapcar #'car (mapcar (lambda (device)
                            (ephone--vcm-interface-send device "GetCalls"))
                          ephone--devices))))

(defun ephone/select-device ()
  "Query available HFP capable Bluetooth device and select one.
  Returns the selected device's ofono object path."
  (ephone--get-hfp-devices)
  (if (not ephone--devices)
      (progn (display-warning "ephone" "No Bluetooth devices found")
             nil)
    ;; Filter out anything that is not /hfp/org/bluez/hci0/dev_*
    ;; Then fetch the name of each device
    (let* ((devices (mapcar (lambda (device)
                              `(,(ephone--get-device-name (ephone--get-bt-address device)) ,device))
                            ephone--devices))
           (path (completing-read "Select a Bluetooth device: " devices)))
      (car (cdr (assoc path devices))))))

(defun ephone--dbus-send(object interface method &rest args)
  "Send a dbus message to ofonod."
  (apply 'dbus-call-method
         :system
         "org.ofono"
         object
         interface
         method
         args))

(defun ephone--vcm-interface-send (object method &rest args)
  "Call a method on ofono's VoiceCallManager-Interface."
  (when (and object method)
    (apply 'ephone--dbus-send
           object
           "org.ofono.VoiceCallManager"
           method
           args)))

(defun ephone--vc-interface-send (object method &rest args)
  "Call a method on ofono's VoiceCall-Interface."
  (when (and object method)
    (apply 'ephone--dbus-send
           object
           "org.ofono.VoiceCall"
           method
           args)))

(defun ephone--maybe-select-device ()
  "Ask the user to select a device if multiple devices are available.
  Otherwise, return the only device available.
  Returns the selected device's ofono object path."
  (ephone--get-hfp-devices) ;; Refresh the list of devices
  (if (> (length ephone--devices) 1)
      (ephone/select-device)
    (car ephone--devices)))

(defun ephone/dial (number)
  "Call NUMBER.
   Returns the D-BUS object path to the voice call."
  (interactive "sNumber: ")
  (let ((device (ephone--maybe-select-device)))
    (if device
        (ephone--vcm-interface-send (ephone--maybe-select-device) "Dial" (ephone--clean-number number) "")
      (progn (display-warning "ephone" "No devices available")
             nil))))

(defun ephone/dial-last-number ()
  "Call the last number dial-ed."
  (interactive)
  (let ((device (ephone--maybe-select-device)))
    (if device
        (ephone--vcm-interface-send device "DialLast")
      (progn (display-warning "ephone" "No devices available")
             nil))))

(defun ephone/answer ()
  "Answer an incoming call."
  (interactive)
  (let ((device (ephone--maybe-select-device)))
    (if device
        (let ((calls (ephone--get-calls)))
          (if calls
              (ephone--vc-interface-send (caar calls) "Answer")
            (progn (display-warning "ephone" "No calls to answer")
                   nil)))
      (progn (display-warning "ephone" "No devices available")
             nil))))

(defun ephone/hangup ()
  "Hang up the current (or incoming) call."
  (interactive)
  (let ((device (ephone--maybe-select-device)))
    (if device
        (let ((calls (ephone--get-calls)))
          (if calls
              (ephone--vc-interface-send (caar calls) "Hangup")
            (progn (display-warning "ephone" "No calls to hangup")
                   nil)))
      (progn (display-warning "ephone" "No devices available")
             nil))))

(defun ephone--handle-modem-added (device &rest properties)
  "Attach D-Bus hooks to the modem at DEVICE (D-Bus object)."
  (add-to-list 'ephone--dbus-hooks (dbus-register-signal :system
                                                         "org.ofono"
                                                         device
                                                         "org.ofono.VoiceCallManager"
                                                         "CallAdded"
                                                         'ephone--call-added))
  (add-to-list 'ephone--dbus-hooks (dbus-register-signal :system
                                                         "org.ofono"
                                                         device
                                                         "org.ofono.VoiceCallManager"
                                                         "CallRemoved"
                                                         'ephone--call-removed)))

(defun ephone--attach-dbus-hooks ()
  "Attach hooks to the D-BUS interface and refresh connected HFP devices."
  (when ephone--dbus-hooks
    (dolist (hook ephone--dbus-hooks)
      (dbus-unregister-object hook)))
  (add-to-list 'ephone--dbus-hooks (dbus-register-signal :system
                                                         "org.ofono"
                                                         "/"
                                                         "org.ofono.Manager"
                                                         "ModemAdded"
                                                         'ephone--handle-modem-added))
  ;; TODO: Support ModemRemoved to remove D-Bus hooks
  (setq ephone--devices (ephone--get-hfp-devices))
  (dolist (device ephone--devices)
    ;; Attach hooks to each device
    (ephone--handle-modem-added device)))

(defun ephone/get-phone-number ()
  "Returns the phone number of the first call active.
   Returns nil if no phone or call is active.
   Returns an empty string when the phone number is unknown to ofonod"
  (if (ephone--maybe-select-device)
      (let* ((object (caar (ephone--get-calls)))
             (properties (ephone--vc-interface-send object "GetProperties")))
        (when (not properties)
          (progn (display-warning "ephone" (format "Could not get properties for '%s'" object))
                 nil))
        ;; The phone number is stored in the dictionary under the key "LineIdentification"
        (caadr (assoc "LineIdentification" properties)))
    (progn (display-warning "ephone" "No devices available")
           nil)))


;;
;; Org-link support for tel: links.
;;
(with-eval-after-load "org"
  (org-link-set-parameters "tel"
                           :follow (lambda (number)
                                     (ephone/dial number))
                           :export (lambda (path desc backend)
                                     (cond
                                      ((eq 'html backend)
                                       (format "<a href=\"tel:%s\">%s</a>" path desc))
                                      ((eq 'latex backend)
                                       (format "\\href{tel:%s}{%s}" path desc))
                                      (t path)))
                           :face '(:foreground "blue" :underline t)))

(provide 'ephone)
