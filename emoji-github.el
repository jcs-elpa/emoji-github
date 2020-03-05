;;; emoji-github.el --- Display list of GitHub's emoji.  (cheat sheet)  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-03-05 10:58:18

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Display list of GitHub's emoji.  (cheat sheet)
;; Keyword: list github emoji display handy
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (emojify "1.0"))
;; URL: https://github.com/jcs090218/emoji-github

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Display list of GitHub's emoji.  (cheat sheet)
;;

;;; Code:

(require 'tabulated-list)

(require 'emojify)


(defgroup emoji-github nil
  "Display list of GitHub's emoji.  (cheat sheet)"
  :prefix "emoji-github-"
  :group 'convenience
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/emoji-github"))

(defcustom emoji-github-columns 3
  "Columns to display in each row."
  :type 'integer
  :group 'emoji-github)

(defconst emoji-github--buffer-name "*GitHub Emoji*"
  "Name of the GitHub emoji buffer.")

(defconst emoji-github--list
  '(;; People
    "bowtie" "smile" "laughing"
    "blush" "smiley" "relaxed"
    "smirk" "heart_eyes" "kissing_heart"
    "kissing_closed_eyes" "flushed" "relieved"
    "satisfied" "grin" "wink"
    "stuck_out_tongue_winking_eye" "stuck_out_tongue_closed_eyes" "grinning"
    "kissing" "kissing_smiling_eyes" "stuck_out_tongue"
    "sleeping" "worried" "frowning"
    "anguished" "open_mouth" "grimacing"
    "confused" "hushed" "expressionless"
    "unamused" "sweat_smile" "sweat"
    "disappointed_relieved" "weary" "pensive"
    "disappointed" "confounded" "fearful"
    "cold_sweat" "persevere" "cry"
    "sob" "joy" "astonished"
    "scream" "neckbeard" "tired_face"
    "angry" "rage" "triumph"
    "sleepy" "yum" "mask"
    "sunglasses" "dizzy_face" "imp"
    "smiling_imp" "neutral_face" "no_mouth"
    "innocent" "alien" "yellow_heart"
    "blue_heart" "purple_heart" "heart"
    "green_heart" "broken_heart" "heartbeat"
    "heartpulse" "two_hearts" "revolving_hearts"
    "cupid" "sparkling_heart" "sparkles"
    "star" "star2" "dizzy"
    "boom" "collision" "anger"
    "exclamation" "question" "grey_exclamation"
    "grey_question" "zzz" "dash"
    "sweat_drops" "notes" "musical_note"
    "fire" "hankey" "poop"
    "shit" "+1" "thumbsup"
    "-1" "thumbsdown" "ok_hand"
    "punch" "facepunch" "fist"
    "v" "wave" "hand"
    "raised_hand" "open_hands" "point_up"
    "raised_hands" "pray" "point_up_2"
    "clap" "muscle" "metal"
    "fu" "walking" "runner"
    "running" "couple" "family"
    "two_men_holding_hands" "two_women_holding_hands" "dancer"
    "dancers" "ok_woman" "no_good"
    "information_desk_person" "raising_hand" "bride_with_veil"
    "person_with_pouting_face" "person_frowning" "bow"
    "couplekiss" "couple_with_heart" "massage"
    "haircut" "nail_care" "boy"
    "girl" "woman" "man"
    "baby" "older_woman" "older_man"
    "person_with_blond_hair" "man_with_gua_pi_mao" "man_with_turban"
    "construction_worker" "cop" "angel"
    "princess" "smiley_cat" "smile_cat"
    "heart_eyes_cat" "kissing_cat" "smirk_cat"
    "scream_cat" "crying_cat_face" "joy_cat"
    "pouting_cat" "japanese_ogre" "japanese_goblin"
    "see_no_evil" "hear_no_evil" "speak_no_evil"
    "guardsman" "skull" "feet"
    "lips" "kiss" "droplet"
    "ear" "eyes" "nose"
    "tongue" "love_letter" "bust_in_silhouette"
    "busts_in_silhouette" "speech_balloon" "thought_balloon"
    "feelsgood" "finnadie" "goberserk"
    "godmode" "hurtrealbad" "rage1"
    "rage2" "rage3" "rage4"
    "suspect" "trollface" ""
    ;; Nature
    "sunny" "umbrella" "cloud"
    "snowflake" "snowman" "zap"
    "cyclone" "foggy" "ocean"
    "cat" "dog" "mouse"
    "hamster" "rabbit" "wolf"
    "frog" "tiger" "koala"
    "bear" "pig" "pig_nose"
    "cow" "boar" "monkey_face"
    "monkey" "horse" "racehorse"
    "camel" "sheep" "elephant"
    "panda_face" "snake" "bird"
    "baby_chick" "hatched_chick" "hatching_chick"
    "chicken" "penguin" "turtle"
    "bug" "honeybee" "ant"
    "beetle" "snail" "octopus"
    "tropical_fish" "fish" "whale"
    "whale2" "dolphin" "cow2"
    "ram" "rat" "water_buffalo"
    "tiger2" "rabbit2" "dragon"
    "goat" "rooster" "dog2"
    "pig2" "mouse2" "ox"
    "dragon_face" "blowfish" "crocodile"
    "dromedary_camel" "leopard" "cat2"
    "paw_prints" "paw_prints" "bouquet"
    "cherry_blossom" "tulip" "four_leaf_clover"
    "rose" "sunflower" "hibiscus"
    "maple_leaf" "leaves" "fallen_leaf"
    "herb" "mushroom" "cactus"
    "palm_tree" "evergreen_tree" "deciduous_tree"
    "chestnut" "seedling" "blossom"
    "ear_of_rice" "shell" "globe_with_meridians"
    "sun_with_face" "full_moon_with_face" "new_moon_with_face"
    "new_moon" "waxing_crescent_moon" "first_quarter_moon"
    "waxing_gibbous_moon" "full_moon" "waning_gibbous_moon"
    "last_quarter_moon" "waning_crescent_moon" "last_quarter_moon_with_face"
    "first_quarter_moon_with_face" "moon" "earth_africa"
    "earth_americas" "earth_asia" "volcano"
    "milky_way" "partly_sunny" "octocat"
    "squirrel" "" ""
    ;; Objects
    "bamboo" "gift_heart" "dolls"
    "school_satchel" "mortar_board" "flags"
    "fireworks" "sparkler" "wind_chime"
    "rice_scene" "jack_o_lantern" "ghost"
    "santa" "christmas_tree" "gift"
    "bell" "no_bell" "tanabata_tree"
    "tada" "confetti_ball" "balloon"
    "crystal_ball" "cd" "dvd"
    "floppy_disk" "camera" "video_camera"
    "movie_camera" "computer" "tv"
    "iphone" "phone" "telephone"
    "telephone_receiver" "pager" "fax"
    "minidisc" "vhs" "sound"
    "speaker" "mute" "loudspeaker"
    "mega" "hourglass" "hourglass_flowing_sand"
    "alarm_clock" "watch" "radio"
    "satellite" "loop" "mag"
    "mag_right" "unlock" "lock"
    "lock_with_ink_pen" "closed_lock_with_key" "key"
    "bulb" "flashlight" "high_brightness"
    "low_brightness" "electric_plug" "battery"
    "calling" "email" "mailbox"
    "postbox" "bath" "bathtub"
    "shower" "toilet" "wrench"
    "moneybag" "yen" "dollar"
    "pound" "euro" "credit_card"
    "money_with_wings" "e-mail" "inbox_tray"
    "outbox_tray" "envelope" "incoming_envelope"
    "postal_horn" "mailbox_closed" "mailbox_with_mail"
    "mailbox_with_no_mail" "door" "smoking"
    "bomb" "gun" "hocho"
    "pill" "syringe" "page_facing_up"
    "page_with_curl" "bookmark_tabs" "bar_chart"
    "chart_with_upwards_trend" "chart_with_downwards_trend" "scroll"
    "clipboard" "calendar" "date"
    "card_index" "file_folder" "open_file_folder"
    "scissors" "pushpin" "paperclip"
    "black_nib" "pencil2" "straight_ruler"
    "triangular_ruler" "closed_book" "green_book"
    "blue_book" "orange_book" "notebook"
    "notebook_with_decorative_cover" "ledger" "books"
    "bookmark" "name_badge" "microscope"
    "" "" ""
    "" "" ""
    "" "" ""
    "" "" ""
    "" "" ""
    "" "" ""
    "" "" ""
    "" "" ""
    "" "" ""
    "" "" ""
    "" "" "")
  "List of GitHub's emoji that we are going to displayed.")

(defun emoji-github--format ()
  "Return the list format from the display emoji."
  (let ((title "List %s")
        (lst '())
        (cnt 1))
    (while (<= cnt emoji-github-columns)
      (push (list (format title cnt) 30 t) lst)
      (setq cnt (1+ cnt)))
    (setq lst (reverse lst))
    (vconcat lst)))

(defun emoji-github--format-item (item)
  "Return the string form by ITEM (emoji)."
  (if (not (string-empty-p item))
      (format ":%s: %s" item item)
    ""))

(defun emoji-github--get-entries ()
  "Get all GitHub's emoji as list entry."
  (let ((entries '()) (index (- (length emoji-github--list) emoji-github-columns)))
    (while (>= index 0)
      (let ((new-entry '()) (new-entry-value '()))
        (let ((col-cnt 0) (col-val nil))
          (while (< col-cnt emoji-github-columns)
            (setq col-val (nth (+ index col-cnt) emoji-github--list))
            (push (emoji-github--format-item col-val) new-entry-value)
            (setq col-cnt (1+ col-cnt))))
        (push (vconcat new-entry-value) new-entry)  ; Turn into vector.
        (push (number-to-string index) new-entry)
        (push new-entry entries))
      (setq index (- index emoji-github-columns)))
    entries))

(define-derived-mode emoji-github-mode tabulated-list-mode
  "emoji-github-mode"
  "Major mode for displaying GitHub's emoji."
  :group 'emp
  (setq tabulated-list-format (emoji-github--format))
  (setq tabulated-list-padding 1)
  (setq-local tabulated-list--header-string "URL: https://gist.github.com/rxaviers/7360908")
  (tabulated-list-init-header)
  (setq tabulated-list-entries (emoji-github--get-entries))
  (tabulated-list-print t)
  (tabulated-list-print-fake-header)
  (emojify-mode 1))

;;;###autoload
(defun emoji-github ()
  "List of GitHub's emoji."
  (interactive)
  (pop-to-buffer emoji-github--buffer-name nil)
  (emoji-github-mode))


(provide 'emoji-github)
;;; emoji-github.el ends here
