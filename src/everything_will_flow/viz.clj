(ns everything-will-flow.core
  (:import
    [java.awt
     Toolkit
     Dimension
     Color]
    [java.awt.event
     KeyEvent]
    [java.awt.image
     BufferedImage]
    [javax.imageio
     ImageIO]
    [javax.swing
     AbstractAction JComponent JFrame JLabel JScrollPane ImageIcon KeyStroke]
    [javax.script
     ScriptEngineManager]))

(def ^:private shortcut-mask
  (.. Toolkit getDefaultToolkit getMenuShortcutKeyMask))

(def ^:private close-key
  (KeyStroke/getKeyStroke KeyEvent/VK_W (int shortcut-mask)))

(defn create-frame
  "Creates a frame for viewing graphviz images.  Only useful if you don't want to use the default frame."
  [name]
  (delay
    (let [frame (JFrame. ^String name)
          image-icon (ImageIcon.)
          pane (-> image-icon JLabel. JScrollPane.)]
      (doto pane
        (.. (getInputMap JComponent/WHEN_IN_FOCUSED_WINDOW)
          (put close-key "closeWindow"))
        (.. getActionMap (put "closeWindow"
                           (proxy [AbstractAction] []
                             (actionPerformed [e]
                               (.setVisible frame false))))))
      (doto frame
        (.setContentPane pane)
        (.setSize 1024 768)
        (.setDefaultCloseOperation javax.swing.WindowConstants/HIDE_ON_CLOSE))
      [frame image-icon pane])))

(defn- send-to-front
  "Makes absolutely, completely sure that the frame is moved to the front."
  [^JFrame frame]
  (doto frame
    (.setExtendedState JFrame/NORMAL)
    (.setAlwaysOnTop true)
    .repaint
    .toFront
    .requestFocus
    (.setAlwaysOnTop false))

  ;; may I one day be forgiven
  (when-let [applescript (.getEngineByName (ScriptEngineManager.) "AppleScript")]
    (try
      (.eval applescript "tell me to activate")
      (catch Throwable e
        ))))

(def default-frame (create-frame "flow"))

(defn view-image
  "Takes an `image`, and displays it in a window.  If `frame` is not specified, then the default frame will be used."
  ([image]
     (view-image default-frame image))
  ([frame image]
     (let [[^JFrame frame ^ImageIcon image-icon ^JLabel pane] @frame]
       (.setImage image-icon image)
       (.setVisible frame true)
       (java.awt.EventQueue/invokeLater
         #(send-to-front frame)))))

(defn ->hsv [t]
  (if (zero? t)
    (Color/getHSBColor 0 0 0)
    (Color/getHSBColor
      (- 0.75 (* 0.85 t))
      1
      1)))

(defn create-image [w h f]
  (let [image (BufferedImage. w h BufferedImage/TYPE_INT_RGB)]
    (dotimes [x w]
      (dotimes [y h]
        (.setRGB image x y (.getRGB ^Color (f x y)))))
    image))
