(ns ghost.core
  (:gen-class main true)
  (:require [clj-slack.rtm :as rtm]
            [clj-slack.channels :as channels]
            [clj-slack.groups :as groups]
            [clj-slack.users :as users]
            [clojure.data.json :as json]
            [gniazdo.core :as ws]))

(defprotocol IBot
  (on-message [this message]))

(defrecord Bot [connection users channels groups event]
  IBot
  (on-message [this message]
    (let [messages (:message (:event this))]
      (doseq [message-fn messages]
        (message-fn message)))))

(defrecord User [id name])

(defrecord Channel [id name members])

(defrecord Group [id name members])

(defn constructor-group [group]
  (let [id (:id group) 
        name (:name group)
        members (:members group)]
    (if id
      (->Group id name members)
      nil)))

(defn constructor-channel [channel]
  (let [id (:id channel)
        name (:name channel)
        members (:members channel)]
    (if id
      (->Channel id name members)
      nil)))

(defn constructor-user
  "create with hash"
 [member]
 (->User (:id member) (:name member)))

(defn create
  ([create-fn data]
   (create create-fn (first data) (rest data)))
  ([create-fn head tail]
   (create create-fn head tail []))
  ([create-fn head tail workpieces]
   (let [workpiece (create-fn head)
        workpieces(if workpiece (conj workpieces workpiece) workpieces)]
     (if-not (empty? tail)
       (create create-fn (rest head) (rest tail) workpieces)
       workpieces))))

(defn constructor-bot
  ([token]
   (constructor-bot token {:message [#(println %)]}))
  ([token event]
   (let [connection {:api-url "https://slack.com/api" :token token}
         users (create #'constructor-user (:members (users/list connection)))
         channels (create #'constructor-channel (:channels (channels/list connection)))
         groups (create #'constructor-group (:groups (groups/list connection)))
         bot (->Bot connection users channels groups event)]
     bot)))
  
(defn on-message [data]
  (println (get data :text)))

(defn on-nop [data] "nop")

(defn select-func [type]
  (case type
    "message" #'on-message
    #'on-nop))

(defn receiver [mes]
  (let [data (json/read-str mes :key-fn keyword)
        type (get data :type)
        on (select-func type)]
    (on data)))

(defn connect [url]
  (let [s (ws/connect url
                      :on-receive #'receiver)]))

(defn -main [& args]
  (let [token (first args)
        connection  {:api-url "https://slack.com/api" :token token}]
    (connect (get (rtm/start connection) :url))))

