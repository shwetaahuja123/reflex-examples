{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Frontend.Examples.BasicToDo.Main
  (app)
  where

{-
 - Stripped version of todo list: just add new todo and delete an old one
 -}

import           Control.Lens
import qualified Data.Map     as M
import qualified Data.Text    as T
import qualified Data.Bool    as B
import           Reflex.Dom
import           Control.Monad.Fix (MonadFix)


app :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => m ()
app = elAttr "div" ("id" =: "todo-background" <>
                "onclick" =: "document.getElementById('todo-input').focus()") $ do
        rec
          let enter = keypress Enter input

              modAttrsEv = getModAttrs <$> (True <$ enter)
              getModAttrs :: B.Bool -> M.Map AttributeName (Maybe T.Text)
              getModAttrs b = "disabled" =: Just (B.bool "false" "true" b)
                              <> "placeholder" =: Just (B.bool "define hunter task" "" b)

          input <- inputElement $ def
            & inputElementConfig_setValue .~ fmap (const "") enter
            & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
              ("id" =: "todo-input"
               <> "placeholder" =: "define hunter task")
            & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modAttrsEv

          let taskEv = tagPromptlyDyn (_inputElement_value input) enter
          divClass "task" $ dynText =<< holdDyn "" taskEv
        pure()
