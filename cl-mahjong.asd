;; cl-mahjong.asd
;;
;; Copyright (C) 2025 George Watson
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(asdf:defsystem #:cl-mahjong
  :description "Riichi Mahjong game logic library"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license  "GPL-3.0"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "tile-utils")
               (:file "hand-analysis")
               (:file "winning-context")
               (:file "yaku-detection")
               (:file "scoring")
               (:file "game-integration")
               (:file "wall")
               (:file "player")
               (:file "game-state")))
