(ns slack-ghost.bot-util
  (:require [clj-slack.channels :as channels]))

(defn search
  ([list target key]
   (search target key (first list) (rest list)))
  ([target key head tail]
   (if head
     (if (= target (key head)) 
       head
       (search target key (first tail) (rest tail))))))

(defn search-user [this data]
  (search (:users this) (:user data) :id))

(defn search-channel [this data]
  (search (:channels this) (:channel data) :id))

(defn search-channel-with-name [this name]
  (search (:channels this) name :name))

(defn match-channel [this data ch-string]
  (let [ch-name (:name (search-channel this data))]
    (= ch-name ch-string)))

(defn join-all
  ([bot]
   (loop [channels (:channels bot)]
     (if (not (= (count channels) 0))
       (do
         (join-all bot (:name (first channels)))
         (recur (rest channels))))))
  ([bot channel-name]
   (channels/join (:connection bot) channel-name)))
