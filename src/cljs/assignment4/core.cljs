(ns assignment4.core
    (:require-macros [reagent.ratom :refer [reaction]])
    (:require
              [schema.core :as s]
              [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [reagent-forms.core :refer [bind-fields init-field value-of]]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]))

;; -------------------------
;; schema start
(def drawing-mode-schema (s/enum :L :R :C))

(def command-schema (s/enum :draw_shape :select-drawing-mode))

(def draw-shape-attribute-schema
                                {
                                   (s/required-key :mode) drawing-mode-schema
                                   (s/required-key :point1) point-schema
                                   (s/required-key :point2) point-schema
                                }
)

(def draw-shape-command-schema
                   {
                      (s/required-key :command ) command-schema
                      (s/required-key :attributes) draw-shape-attribute-schema
                   }
)

(def select-draw-mode-attribute-schema
          {
             (s/required-key :old-value) s/Str
             (s/required-key :new-value) s/Str
          }
)

(def select-draw-mode-command-schema
                   {
                      (s/required-key :command ) command-schema
                      (s/required-key :attributes) select-draw-mode-attribute-schema
                   }
)

(def point-schema {
                      (s/required-key :x) s/Num
                      (s/required-key :y) s/Num
                  }
)

(def app-state-schema  {
                          (s/required-key :drawing-mode) drawing-mode-schema
                          (s/required-key :commands) [(draw-shape-command-schema, select-draw-mode-command-schema)]
                          (s/required-key :start_point) point-schema
                          (s/required-key :cur_mouse_pos) point-schema
                        }
)

;; -------------------------
;;defining app-state ratom
(def app-state (reagent/atom {
                                :drawing-mode :L
                                :commands []
                                :start_point {:x -1 :y -1}
                                :cur_mouse_pos {:x 0 :y 0}
                             }
               )
)

(def drawing-mode (reagent/cursor app-state [:drawing-mode]))
(def commands (reagent/cursor app-state [:commands]))
(def cur_mouse_pos  (reagent/cursor app-state [:cur_mouse_pos]))

(def start_point  (reagent/cursor app-state [:start_point]))


(defn push-command [command ]
  (swap! commands conj command)
)

(defn pop-command []
  (let [ret (last @commands)]
	 (swap! commands butlast)
	 ret)
)

(defn on-select-drawing-mode
  [value]

  (do
    ( push-command {
                      :command :select-drawing-mode
                      :attributes {:old-value @drawing-mode :new-value value}
                   }
    )
    (reset! drawing-mode value)
  )
)

(defn undo []

  (if (> (count @commands) 0)
    (let [
            command (pop-command)
            cmd (get command :command)
            attrs (get command :attributes)
         ]

        (if (= cmd :select-drawing-mode) (reset! drawing-mode (get attrs :old-value)))
    )
  )
)

(defn drawing-pallete []

   [:div

      [:input {:type "button" :value (str "Line" (if (= :L @drawing-mode) "!")) :on-click #(on-select-drawing-mode :L) }]
      [:input {:type "button" :value (str "Rectangle" (if (= :R @drawing-mode) "!")) :on-click #(on-select-drawing-mode :R) }]
      [:input {:type "button" :value (str "Circle" (if (= :C @drawing-mode) "!")) :on-click #(on-select-drawing-mode :C) }]
   ]
)

(defn controls []
  [:div
     [:input {:type "button" :value "Undo" :on-click #(undo) }]
  ]
)

(defn is_start_point_not_selected []
    (not= -1 (get @start_point :x))
)

(defn on-mouse-click [event]

  (do
    ;; if start is not selected, set start and do nothing
    (if (= false (is_start_point_not_selected))
      (do
        (reset! start_point {:x  (.-clientX event) :y (.-clientY event)})
      )

      (do
        (push-command {
                         :command :draw_shape
                         :attributes
                           {
                              :mode @drawing-mode
                              :point1 {
                                        :x (get @start_point :x) :y (get @start_point :y)
                                      }
                              :point2 {
                                        :x (get @cur_mouse_pos :x) :y (get @cur_mouse_pos :y)
                                      }

                           }
                      }
        )

        (reset! start_point {:x -1 :y -1})
      )
    )
  )
)

(defn on-mouse-move [event]
  (reset! cur_mouse_pos {:x  (.-clientX event) :y (.-clientY event)})
)

(defn distance-between [x1 y1 x2 y2]
  (let [dx (- x2 x1) dy (- y2 y1)]
   (.sqrt js/Math (+ (* dx dx) (* dy dy)))
  )
)

(defn draw-shape
  "Draws the corresponding shape if the command type is draw_shape"
  [command]

  (if  (= (-> command :command) :draw_shape)
      (let [
              mode (-> command :attributes :mode)
              x1 (-> command :attributes :point1 :x)
              y1 (-> command :attributes :point1 :y)
              x2 (-> command :attributes :point2 :x)
              y2 (-> command :attributes :point2 :y)
           ]

        (case mode
          :L
            [:line {
                    :x1 x1
                    :y1 y1
                    :x2 x2
                    :y2 y2
                 }
            ]

          :R
            [:rect {
                    :x (if (< x1 x2) x1 x2)
                    :y (if (< y1 y2) y1 y2)
                    :width (.abs js/Math (- x1 x2))
                    :height (.abs js/Math (- y1 y2))
                 }
            ]
          :C
            [:circle {
                  :cx x1
                  :cy y1
                  :r (distance-between x1 y1 x2 y2)
               }
            ]
        )

      )
  )
)

(defn draw-current-shape
    "Draws the current shape if the start point is selected with current mouse position as the second point"
    []
    (if (is_start_point_not_selected)
        [draw_shape {
                      :command :draw_shape
                      :attributes
                      {
                       :mode @drawing-mode
                       :point1 {
                                 :x (get @start_point :x) :y (get @start_point :y)
                               }
                       :point2 {
                                 :x (get @cur_mouse_pos :x) :y (get @cur_mouse_pos :y)
                               }
                      }
                    }
        ]
    )
)

(defn drawing-area
  "Renders svg element which includes drawing the current shape and iterating the commands stack to render past shapes"
  []
  [:svg {
           :width 800
           :height 500
           :stroke "red"
           :fill "white"
           :fill-opacity 0.0
           :on-click #(on-mouse-click %)
           :on-mouse-move #(on-mouse-move %)
           :style {:position :fixed :top 0 :left 0 :border "black solid 1px"}
        }

       [draw-current-shape]

       (for [command @commands]
         [draw_shape command]
       )
  ]
)

(defn display-state []

  [:div
    [:p "Command count :"(count @commands)]
    [:p "Drawing mode "(str @drawing-mode)]
    [:p "X :"(get @cur_mouse_pos :x)]
    [:p "Y :"(get @cur_mouse_pos :y)]
  ]
)

(defn home-page []
    [:body
      [:nav "Pallete" [drawing-pallete] [:hr] [controls] [:hr] [display-state]]
      [:section  [drawing-area]]
    ]
)

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page)
)

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!)
  (accountant/dispatch-current!)
  (mount-root))
