(ns it.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [secretary.core :as secretary :include-macros true :refer [defroute]]
   [cljs.core.async :as async :refer [>! <! put! chan close! pub sub]]
   [it.events :as events :refer [listen drag]]
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [figwheel.client :as fw]))


(enable-console-print!)

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

(def app-state 
  (atom 
    {:menu {:current-page 0
            :items [{:title "page1" :page-id :title-view}
                    {:title "page2" :page-id :image-view}
                    {:title "page3" :page-id :list-view}]

            :title-view {:state {:title "This is a title page"}
                         :component title-view}
            :image-view {:state {:src "../img/index.jpe"}
                         :component image-view}
            :list-view {:state {:class "centered"
                               :items [{:title "foo"} {:title "bar"} {:title "baz"}]}
                        :component list-view}}}))

(defn animate [owner attr target-value duration ease]
  (let [interpolator (.interpolate js/d3 (om/get-state owner attr), target-value)]
    (-> (.transition js/d3)
      (.duration 200)
      (.ease "cubic-out")
      (.tween attr (fn [] (fn [t] (om/set-state! owner attr (interpolator t))))))
    ))

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
                           (dom/li #js{:onClick #(set! (.-hash js/location) href)
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
                             (> (:vx drag-event) 1) (>! pub-chan {:topic :history :data "/menu/on"})
                             (< (:vx drag-event) -1) (>! pub-chan {:topic :history :data "/menu/off"})
                             (> (+ menu-width old-x) (/ menu-width 2)) (>! pub-chan {:topic :history :data "/menu/on"})
                             :else (>! pub-chan {:topic :history :data "/menu/on"})))
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
                (if (= (:data (<! menu-events)) true)
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
                #js{:id "hamburger-btn" :onClick #(put! pub-chan {:topic :history :data "menu/on"}})} "\u2630"))
            (om/build main-page menu-state)
            (let [percent-showing (/ (- menu-width (.abs js/Math menu-x-pos)) menu-width)
                  menu-on? (if (> percent-showing 0) true false)]
              (dom/div #js{:style #js{:opacity percent-showing
                                      :visibility (if (> percent-showing 0) "visible" "hidden")}
                           :className "main-page-overlay"
                           :onClick #(put! pub-chan {:topic :history :data "/menu/on"})})))
          (dom/div #js{:id "left-slide-menu" 
                       :style #js{:transform (str "translate3d(" menu-x-pos "px, 0, 0)")}
                       :ref "slide-menu"} 
            (om/build menu menu-state)))))))

(defn app [app-state owner]
  (reify
    om/IRender
    (render [_]
      (om/build menu-view (:menu app-state)))))

(def pub-chan (chan))
(def notif-chan (pub pub-chan :topic))

(om/root app app-state
  {:shared {:pub-chan pub-chan
            :notif-chan notif-chan}
   :target (. js/document (getElementById "app")) })

;(let [app-history (sub notif-chan :history (chan))
      ;nav-history (events/history-chan)]
  ;(go (loop []
        ;(let [[v c] (alts! [app-history nav-history])]
          ;(condp = c
            ;
            ;
              ;))))
;)
;(secretary/set-config! :prefix "#")

;(let [chans (nav-chans)]
  ;(def nav-out (:out chans))
  ;(def nav-in (:in chans)))

                                        ;(defroute page-paths "/pages/:id" [id]
                                        ;(swap! app-state update-in [:main-page] assoc :current-page (keyword id)))

                                        ;(defroute page-paths "menu"
                                        ;(swap! app-state update-in [:main-page] assoc :current-page (keyword id)))

;(go (loop []
      ;(let [e (<! nav-out)]
        ;(secretary/dispatch! (.-token e)))
      ;(recur)))

;;; Figwheel
(fw/watch-and-reload
  :jsload-callback (fn []))
