{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.CadCliente where

import Import
import LayoutPark
import Data.Maybe


formCliente :: Form Cliente 
formCliente = renderDivs $ Cliente
    <$> areq textField "Nome " Nothing
    <*> areq textField "CNH " Nothing
    <*> aopt dayField "Data de Nascimento " Nothing
    <*> aopt intField "Telefone 1 " Nothing
    <*> aopt intField "Telefone 2 " Nothing

getCadClienteR :: Handler Html
getCadClienteR = undefined

postCadClienteR :: Handler Html
postCadClienteR = undefined