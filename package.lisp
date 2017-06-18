(defpackage #:coremidi
  (:use #:cl)
  (:export
    ;; From cffi.lisp:
    ;; Data Types
    #:object-ref
    #:device-ref
    #:entity-ref
    #:endpoint-ref
    #:client-ref
    #:port-ref
    #:time-stamp
    ;; MIDI Devices
    #:device-get-entity
    #:device-get-number-of-entities
    #:get-device
    #:get-number-of-devices
    ;; MIDI External Devices
    #:get-external-device
    #:get-number-of-external-devices
    ;; MIDI Entities
    #:entity-get-destination
    #:entity-get-device
    #:entity-get-number-of-destinations
    #:entity-get-number-of-sources
    #:entity-get-source
    ;; MIDI Endpoints
    #:destination-create
    #:endpoint-dispose
    #:endpoint-get-entity
    #:get-destination
    #:get-number-of-destinations
    #:get-number-of-sources
    #:get-source
    #:source-create
    ;; MIDI Ports
    #:input-port-create
    #:output-port-create
    #:port-connect-source
    #:port-disconnect-source
    #:port-dispose
    ;; MIDI Packet Lists
    #:packet-list-add
    #:packet-list-init
    ;; MIDI Objects and Properties
    #:object-find-by-unique-id
    #:object-string-property
    ; MIDI I/O
    #:flush-output
    #:received
    #:rescan
    #:send
    #:send-sysex
    ; MIDI Clients
    #:client-create
    #:client-dispose

    ;; From coremidi.lisp:
    ;; System-Wide Utilities
    #:display-name
    #:system-sources
    #:system-destinations
    #:print-system-sources
    #:print-system-destinations
    #:find-system-sources
    #:find-system-destinations
    ;; The Client Structure
    #:*midi-client*
    ;; Incoming Messages Processing
    #:+note-off+
    #:+note-on+
    #:+polyphonic-aftertouch+
    #:+control/mode-change+
    #:+program-change+
    #:+channel-aftertouch+
    #:+pitch-wheel-range+
    #:+system-exclusive+
    #:+song-position-pointer+
    #:+song-select+
    #:+tune-request+
    #:+end-of-system-exclusive+
    #:+timing-clock+
    #:+start+
    #:+continue+
    #:+stop+
    #:+active-sensing+
    #:+system-reset+
    #:register-message-handler
    ;; Outgoing Message Processing
    #:now
    #:message-at
    #:message
    ;; CoreMIDI Notifications Processing
    #:+setup-changed+
    #:+object-added+
    #:+object-removed+
    #:+property-changed+
    #:+thru-connections-changed+
    #:+serial-port-owner-changed+
    #:+io-error+
    #:register-notification-handler
    ;; Client Maintenance
    #:initialize
    #:dispose))
