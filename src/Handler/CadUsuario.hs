{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.CadUsuario where

import Import
import LayoutPark

formUsuario :: Form Usuario 
formUsuario = renderDivs $ Usuario
    <$> areq textField "Nome " Nothing
    <*> areq textField "Login " Nothing
    <*> areq passwordField "Senha " Nothing


getCadUsuarioR :: Handler Html
getCadUsuarioR = undefined

postCadUsuarioR :: Handler Html
postCadUsuarioR = undefined