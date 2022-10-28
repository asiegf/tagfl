(ns tagfl.config)

(def image-names
  {#{:abstract}       "abstract.svg"
   #{:abstract :user} "abstract-user.svg"
   #{:input :user}    "input.svg"})

(def operators {:enable            ">>"
                :concurrent        "|||"
                :choice            "?"
                :disable           "!"
                :suspend-resume    "|>"
                :order-independent "|-|"})
