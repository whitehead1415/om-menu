(ns om-menu.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [secretary.core :as secretary :include-macros true :refer [defroute]]
   [cljs.core.async :as async :refer [>! <! put! chan close!]]
   [goog.events :as events]
   [goog.history.EventType :as HistoryEventType]
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [figwheel.client :as fw])
  (:import goog.History
           [goog.events EventType]))


(enable-console-print!)

;;; Events


(def classes
  {:menu-on "show-menu"
   :menu-off "hide-menu"})

(def keyword->event-type
  {:mousedown EventType/MOUSEDOWN
   :mouseup EventType/MOUSEUP
   :mousemove EventType/MOUSEMOVE
   :resize EventType/RESIZE})

(defn listen
  ([el type] (listen el type nil))
  ([el type f] (listen el type f (chan)))
  ([el type f out]
     (events/listen el (keyword->event-type type)
                    (fn [e] (when f (f e)) (put! out e)))
     out))

(defn drag [el start-predicate]
  (let [out (chan)
        start-chan (listen el :mousedown)
        move-chan (listen el :mousemove)
        end-chan (listen el :mouseup)]
    (go (loop [engaged? false moved? false]
          (let [[v c] (alts! [start-chan move-chan end-chan])]
            (condp = c
              start-chan (if (start-predicate v) 
                           (do 
                             (>! out {:type :start :x (.-clientX v) :y (.-clientY v)})
                             (recur true false))
                           (recur false false))
              end-chan (do 
                         (when (and moved? engaged?)
                           (>! out {:type :end :x (.-clientX v) :y (.-clientY v)}))
                         (recur false false))
              move-chan (do
                          (when engaged? (>! out {:type :move :x (.-clientX v) :y (.-clientY v)}))
                          (recur engaged? true)))
            (close! out))))
    out))


;;; Model

(def css-trans-group (-> js/React (aget "addons") (aget "CSSTransitionGroup"))) 

(def app-state (atom {:main-page {:current-page :title-view
                                  :pages {:title-view {:type "title-view" :title "This is a title page"}
                                          :image-view {:type "image-view" :src "../img/index.jpe"}
                                          :list-view {:type "list-view" :class "centered"
                                                      :items [{:title "foo"} {:title "bar"} {:title "baz"}]}}}
                      :menu {:items [{:title "page1" :href "#pages/title-view"
                                      :class "side-menu-list-item"}
                                     {:title "page2" :href "#pages/image-view"
                                      :class "side-menu-list-item"}
                                     {:title "page3" :href "#pages/list-view"
                                      :class "side-menu-list-item"}]}}))

;;; Views

(defn list-item [item owner]
  (reify
    om/IRender
    (render [this]
      (let [href (:href item)
            title (:title item)
            class (:class item)]
        (dom/li #js{:onClick #(set! (.-hash js/location) href)
                    :className class} title)))))

(defn list-view [page owner]
  (reify
    om/IRender
    (render [this]
      (apply dom/ul #js{:className (:class page)}
             (om/build-all list-item (:items page))))))

(defn image-view [page owner]
  (reify
    om/IRender
    (render [this]
      (dom/img #js{:className "centered" :src (:src page)} nil))))

(defn title-view [page owner]
  (reify
    om/IRender
    (render [this]
      (dom/h1 #js{:className "page-title"} (:title page)))))

(defn menu [menu-state owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js{:className "menu"}
               (om/build list-view menu-state)))))

(defn main-page [main-page-state owner]
  (reify
    om/IRender
    (render [this]
      (let [current-page-symbol (:current-page main-page-state)
            page (current-page-symbol (:pages main-page-state))
            type (:type page)]
        (cond
         (= type "title-view") (om/build title-view page)
         (= type "image-view") (om/build image-view page)
         (= type "list-view") (om/build list-view page))))))

(defn app [app-state owner]
  (reify
    om/IInitState
    (init-state [this] 
      {:menu-on? false 
       :menu-chan (chan)
       :resize-chan (chan)
       :menu-x-pos: 0
       :menu-width: 0
       :drag-start-point 0
       :drag-events (drag js/document
                          (fn [e] 
                            (let [x (.-clientX e)
                                  menu-width (om/get-state owner :menu-width)
                                  x-pos (om/get-state owner :menu-x-pos)]
                              (if (= x-pos 0)
                                (and (> x 0) (< x (+ x-pos menu-width)))
                                (and (> x 0) (< x 20))))))})

    om/IWillMount
    (will-mount [_]
      (let [menu-chan (om/get-state owner :menu-chan)
            drag-chan (om/get-state owner :drag-events)
            resize-chan (om/get-state owner :resize-chan)]
        (go (loop []
              (if (= (<! menu-chan) true)
                (om/set-state! owner :menu-x-pos 0)
                (om/set-state! owner :menu-x-pos (- (om/get-state owner :menu-width))))
              (recur)))
        (go (loop []
              (let [drag-event (<! drag-chan)
                    x (:x drag-event)
                    old-x (om/get-state owner :menu-x-pos)
                    menu-width (om/get-state owner :menu-width)]
                (condp = (:type drag-event)
                  :start (om/set-state! owner :drag-start-point (.abs js/Math (- old-x (- x menu-width))))
                  :end (do
                         (om/set-state! owner :drag-start-point 0)
                         (if (< x (/ menu-width 2)) 
                           (>! menu-chan false)
                           (>! menu-chan true)))
                  (when (< (+ x (om/get-state owner :drag-start-point)) menu-width)
                      (om/set-state! owner :menu-x-pos (+ 
                                                        (- x menu-width)
                                                        (om/get-state owner :drag-start-point))))))
              (recur)))
        (go (loop []
              (let [resize-event (<! resize-chan)
                    new-width (.-offsetWidth (om/get-node owner "slide-menu"))]
                (om/set-state! owner :menu-width new-width))
              (recur)))))
    
    om/IDidMount
    (did-mount [this]
      (let [menu-width (.-offsetWidth (om/get-node owner "slide-menu"))]
        (om/set-state! owner :menu-width menu-width)
        (om/set-state! owner :menu-x-pos (- menu-width))
        (listen js/window :resize nil (om/get-state owner :resize-chan))))

    om/IRenderState
    (render-state [this {:keys [menu-chan menu-on? menu-x-pos]}]
      (dom/div #js{:className "full-height"}
               (dom/div #js{:id "main-page-wrapper" :className (if menu-on? "menu-on" "menu-off")}
                        (dom/div #js{:id "top-bar"}
                                 (dom/button #js{:id "hamburger-btn"
                                                 :onClick #(put! menu-chan true)} 
                                             "\u2630"))
                        (om/build main-page (:main-page app-state))
                        (let [class "main-page-overlay"]
                          (dom/div #js{:className (if menu-on? class (str class " hide")) 
                                       :onClick #(put! menu-chan false)})))
               (dom/div #js{:id "left-slide-menu" 
                            :style #js{:transform (str "translate3d(" menu-x-pos "px, 0, 0)")}
                            :ref "slide-menu"} 
                        (om/build menu (:menu app-state)))))))

(def resize (listen js/window :resize))

(om/root app app-state
         {:target (. js/document (getElementById "app")) })

;;; Controls

(secretary/set-config! :prefix "#")

(defroute page-paths "/pages/:id" [id]
  (swap! app-state update-in [:main-page] assoc :current-page (keyword id)))

(let [h (History.)]
  (goog.events/listen h HistoryEventType/NAVIGATE #(secretary/dispatch! (.-token %)))
  (doto h (.setEnabled true)))

;;; Figwheel
(fw/watch-and-reload
 :jsload-callback (fn []))
