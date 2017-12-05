{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Impressao where

import Import
import LayoutPark

tipoVeicTicket :: TipoveiculoId -> Widget
tipoVeicTicket tvid = do 
    tvei <- handlerToWidget $ runDB $ get404 tvid
    [whamlet|
        <p>Tipo:&nbsp;&nbsp;  #{tipoveiculoNome tvei}
    |]