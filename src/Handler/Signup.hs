{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Signup where

import Import
import Forms

postSignupR :: Handler Html
postSignupR = do
    ((result, _), _) <- runFormPost signupForm

    case result of
        FormSuccess signup -> do
            void $ runDB $ insert signup
            setMessage "Thank you for your interest! You'll get an email when we launch."
            redirect HomeR

        _ -> do
            setMessage "Invalid email."
            redirect HomeR
