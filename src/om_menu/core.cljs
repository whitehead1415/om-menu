(ns om-menu.core
  (:require
   [secretary.core :as secretary :include-macros true :refer [defroute]]
   [goog.events :as events]
   [goog.history.EventType :as EventType]
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [figwheel.client :as fw])
  (:import goog.History))


(enable-console-print!)

;;; Model

(def css-trans-group (-> js/React (aget "addons") (aget "CSSTransitionGroup"))) 

(def app-state (atom {:main-page {:current-page :title-view
                                  :pages {:title-view {:type "title-view" :title "This is a title page"}
                                          :image-view {:type "image-view" :src "../img/index.jpe"}
                                          :list-view {:type "list-view" :class "centered"
                                                      :items [{:title "foo"} {:title "bar"} {:title "baz"}]}}}
                      :show-menu? false
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
    om/IRender
    (render [this]
      (let [show-menu? (:show-menu? app-state)
            show-menu-class (if show-menu? "show-menu" "hide-menu")]
        (dom/div #js{:className "full-height"} 
                 (dom/div #js{:id "main-page-wrapper" :className show-menu-class}
                          (dom/div #js{:id "top-bar"}
                                   (dom/button #js{:id "hamburger-btn"
                                                   :onClick #(om/update! app-state :show-menu? (not show-menu?))} 
                                               "\u2630"))
                          (om/build main-page (:main-page app-state))
                          (let [class "main-page-overlay"]
                            (dom/div #js{:className (if show-menu? class (str class " hide")) 
                                         :onClick #(om/update! app-state :show-menu? (not show-menu?))})))
                 (dom/div #js{:id "left-slide-menu" :className show-menu-class} 
                          (om/build menu (:menu app-state))))))))

(om/root app app-state
         {:target (. js/document (getElementById "app")) })

;;; Controls

(secretary/set-config! :prefix "#")

(defroute page-paths "/pages/:id" [id]
  (swap! app-state update-in [:main-page] assoc :current-page (keyword id)))

(let [h (History.)]
  (goog.events/listen h EventType/NAVIGATE #(secretary/dispatch! (.-token %)))
  (doto h (.setEnabled true)))

;;; Figwheel
(fw/watch-and-reload
 :jsload-callback (fn []))
