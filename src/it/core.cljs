(ns it.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [secretary.core :as secretary :include-macros true :refer [defroute]]
   [cljs.core.async :as async :refer [>! <! put! chan close!]]
   [it.events :as events :refer [listen drag nav-chans]]
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [figwheel.client :as fw]))


(enable-console-print!)

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

(defn animate [owner attr target-value duration ease]
  (let [interpolator (.interpolate js/d3 (om/get-state owner attr), target-value)]
    (-> (.transition js/d3)
      (.duration 200)
      (.ease "cubic-out")
      (.tween attr (fn [] (fn [t] (om/set-state! owner attr (interpolator t))))))
    ))

(defn menu-view [app-state owner]
  (reify
    om/IInitState
    (init-state [this] 
      {:menu-chan (chan)
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
                (do 
                  (animate owner :menu-x-pos 0))
                (animate owner :menu-x-pos (- (om/get-state owner :menu-width))))
              (recur)))
        (go (loop []
              (let [drag-event (<! drag-chan)
                    x (:x drag-event)
                    old-x (om/get-state owner :menu-x-pos)
                    menu-width (om/get-state owner :menu-width)]
                (condp = (:type drag-event)
                  :start (om/set-state! owner :drag-start-point (.abs js/Math (- old-x (- x menu-width))))
                  :end (do (om/set-state! owner :drag-start-point 0)
                           (cond 
                             (> (:vx drag-event) 1) (>! menu-chan true)
                             (< (:vx drag-event) -1) (>! menu-chan false)
                             (> (+ menu-width old-x) (/ menu-width 2)) (>! menu-chan true)
                             :else (>! menu-chan false)))
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
    (render-state [this {:keys [menu-chan menu-x-pos menu-width]}]
      (dom/div #js{:className "full-height"}
        (dom/div #js{:id "main-page-wrapper"}
          (dom/div #js{:id "top-bar"}
            (dom/button 
              #js{:id "hamburger-btn" :onClick #(put! menu-chan true)} "\u2630"))
          (om/build main-page (:main-page app-state))
          (let [percent-showing (/ (- menu-width (.abs js/Math menu-x-pos)) menu-width)
                menu-on? (if (> percent-showing 0) true false)]
            (dom/div #js{:style #js{:opacity percent-showing
                                    :visibility (if (> percent-showing 0) "visible" "hidden")}
                         :className "main-page-overlay"
                         :onClick #(put! menu-chan false)})))
        (dom/div #js{:id "left-slide-menu" 
                     :style #js{:transform (str "translate3d(" menu-x-pos "px, 0, 0)")}
                     :ref "slide-menu"} 
          (om/build menu (:menu app-state)))))))

(om/root menu-view app-state {:target (. js/document (getElementById "app")) })

(secretary/set-config! :prefix "#")

(let [chans (nav-chans)]
  (def nav-out (:out chans))
  (def nav-in (:in chans)))

(defroute page-paths "/pages/:id" [id]
  (swap! app-state update-in [:main-page] assoc :current-page (keyword id)))

                                        ;(defroute page-paths "menu"
                                        ;(swap! app-state update-in [:main-page] assoc :current-page (keyword id)))

(go (loop []
      (let [e (<! nav-out)]
        (secretary/dispatch! (.-token e)))
      (recur)))

;;; Figwheel
(fw/watch-and-reload
  :jsload-callback (fn []))
