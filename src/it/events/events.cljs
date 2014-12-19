(ns it.events
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [>! <! put! chan close! sub]]
            [goog.events :as events]
            [goog.history.EventType :as HistoryEventType]
            [secretary.core :as secretary])
  (:import goog.History
           [goog.events EventType]))

(def keyword->event-type
  {:mousedown EventType/MOUSEDOWN
   :mouseup EventType/MOUSEUP
   :mousemove EventType/MOUSEMOVE
   :touchstart EventType/TOUCHSTART
   :touchend EventType/TOUCHEND
   :touchmove EventType/TOUCHMOVE
   :resize EventType/RESIZE
   :history HistoryEventType/NAVIGATE})

(defn listen
  ([el type] (listen el type nil))
  ([el type f] (listen el type f (chan)))
  ([el type f out]
     (events/listen el (keyword->event-type type)
       (fn [e] (when f (f e)) (put! out e)))
     out))

(defn drag 
  ([el] (drag el #(true)))
  ([el start-predicate] (drag el start-predicate (chan)))
  ([el start-predicate out]
     (let [start-chan (listen el :touchstart)
           move-chan (listen el :touchmove)
           end-chan (listen el :touchend)]
       (go (loop [engaged? false moved? false old-time nil old-x nil old-y nil old-vx nil old-vy nil]
             (let [[v c] (alts! [start-chan move-chan end-chan])
                   x (.-clientX (aget (.-changedTouches (.-event_ v)) 0))
                   y (.-clientY (aget (.-changedTouches (.-event_ v)) 0))
                   now (.now js/Date)]
               (condp = c
                 start-chan (if (start-predicate v) 
                              (do (>! out {:type :start :x x :y y})
                                  (recur true false now x y nil nil))
                              (recur false false nil nil nil nil nil))
                 end-chan (do (when (and moved? engaged?)
                                (>! out {:type :end :x x :y y :vx old-vx :vy old-vy}))
                              (recur false false nil nil nil nil nil))
                 move-chan (do (if engaged? 
                                 (let [interval (- now old-time)
                                       vx (/ (- x old-x) interval)
                                       vy (/ (- y old-y) interval)]
                                   (>! out {:type :move :x x :y y :vx vx :vy vy})
                                   (recur engaged? true now x y vx vy))
                                 (recur false false nil nil nil nil nil))))
               (close! out))))
       out)))


(defn nav-chan []
  (let [h (History.)
        nav-chan (listen h :history)]
    (doto h (.setEnabled true))
    (go (loop []
          (let [e (<! nav-chan)]
            (secretary/dispatch! (.-token e)))
          (recur)))))
