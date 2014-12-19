(ns it.views
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :as async :refer [>! <! put! chan close! pub sub]]
   [it.events :as events :refer [listen drag]]
   [it.graphics :as graphics :refer [animate]]
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]))

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

(defn menu [{:keys [current-page items]} owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js{:className "menu"}
        (apply dom/ul nil
          (map-indexed (fn [idx item] 
                         (let [href (:href item)
                               title (:title item)
                               base-class "side-menu-list-item"
                               class (if (= idx current-page) (str base-class " active") base-class)]
                           (dom/li #js{:onClick #(set! (.-hash js/location) (str "/pages/" idx))
                                       :className class} title))) items))))))

(defn main-page [menu-state owner]
  (reify
    om/IRender
    (render [this]
      (let [page-id (:page-id (nth (:items menu-state) (:current-page menu-state)))
            page (page-id menu-state)]
        (om/build (:component page) (:state page))))))

(defn menu-view [menu-state owner]
  (reify
    om/IInitState
    (init-state [this] 
      {:resize-chan (chan)
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
      (let [pub-chan (om/get-shared owner :pub-chan)
            drag-chan (om/get-state owner :drag-events)
            resize-chan (om/get-state owner :resize-chan)]
        (go (loop []
              (let [drag-event (<! drag-chan)
                    x (:x drag-event)
                    old-x (om/get-state owner :menu-x-pos)
                    menu-width (om/get-state owner :menu-width)]
                (condp = (:type drag-event)
                  :start (om/set-state! owner :drag-start-point (.abs js/Math (- old-x (- x menu-width))))
                  :end (do (om/set-state! owner :drag-start-point 0)
                           (cond 
                             (> (:vx drag-event) 1) (>! pub-chan {:topic :menu :data :on})
                             (< (:vx drag-event) -1) (>! pub-chan {:topic :menu :data :off})
                             (> (+ menu-width old-x) (/ menu-width 2)) (>! pub-chan {:topic :menu :data :on})
                             :else (>! pub-chan {:topic :menu :data :off})))
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
      (let [menu-width (.-offsetWidth (om/get-node owner "slide-menu"))
            menu-events (sub (:notif-chan (om/get-shared owner)) :menu (chan))]
        (go (loop []
              (let []
                (if (= (:data (<! menu-events)) :on)
                  (do 
                    (animate owner :menu-x-pos 0))
                  (animate owner :menu-x-pos (- (om/get-state owner :menu-width))))
                (recur))))
        (om/set-state! owner :menu-width menu-width)
        (om/set-state! owner :menu-x-pos (- menu-width))
        (listen js/window :resize nil (om/get-state owner :resize-chan))))

    om/IRenderState
    (render-state [this {:keys [menu-x-pos menu-width]}]
      (let [pub-chan (:pub-chan (om/get-shared owner))]
        (dom/div #js{:className "full-height"}
          (dom/div #js{:id "main-page-wrapper"}
            (dom/div #js{:id "top-bar"}
              (dom/button 
                #js{:id "hamburger-btn" :onClick #(put! pub-chan {:topic :menu :data :on})} "\u2630"))
            (om/build main-page menu-state)
            (let [percent-showing (/ (- menu-width (.abs js/Math menu-x-pos)) menu-width)
                  menu-on? (if (> percent-showing 0) true false)]
              (dom/div #js{:style #js{:opacity percent-showing
                                      :visibility (if (> percent-showing 0) "visible" "hidden")}
                           :className "main-page-overlay"
                           :onClick #(put! pub-chan {:topic :menu :data :off})})))
          (dom/div #js{:id "left-slide-menu" 
                       :style #js{:transform (str "translate3d(" menu-x-pos "px, 0, 0)")}
                       :ref "slide-menu"} 
            (om/build menu menu-state)))))))
