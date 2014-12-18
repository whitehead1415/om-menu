(ns it.events
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [>! <! put! chan close!]]
            [goog.events :as events]
            [goog.history.EventType :as HistoryEventType])
  (:import goog.History
           [goog.events EventType]))

(def keyword->event-type
  {:mousedown EventType/MOUSEDOWN
   :mouseup EventType/MOUSEUP
   :mousemove EventType/MOUSEMOVE
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
  ([el start-predicate]
     (let [out (chan)
           start-chan (listen el :mousedown)
           move-chan (listen el :mousemove)
           end-chan (listen el :mouseup)]
       (go (loop [engaged? false moved? false old-time nil old-x nil old-y nil old-vx nil old-vy nil]
             (let [[v c] (alts! [start-chan move-chan end-chan])
                   x (.-clientX v)
                   y (.-clientY v)
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


(defn nav-chans [pub-chan]
  (let [h (History.)
        in (chan)]
    (doto h (.setEnabled true))
    (go (loop []
          (if-let [token (<! in)]
            (doto h (.setToken token)))
          (recur))
        (close! in))
    {:in in :out (listen h :history)}))
