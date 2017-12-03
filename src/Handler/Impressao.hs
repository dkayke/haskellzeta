{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Impressao where

import Import
import LayoutPark
import Control.Exception.Base
import Data.Time.Format

getImpressaoSaR :: SaidaId -> Handler Html
getImpressaoSaR sid = do 
    imp <- runDB $ selectFirst [SaidaId ==. sid][] :: Handler (Maybe (Entity Saida))
    layoutTicket "ImpressaoSaR"
        [whamlet|
            <p>
                <b>HASK PARK
            $maybe (Entity said ticket) <- imp 
                <p class="title-ticket">HORÁRIOS
                <p>Entrada:&nbsp; #{(formatTime defaultTimeLocale "%d/%m/%Y às %T" (saidaHrentrada ticket))}
                <p>Saída:&nbsp;&nbsp;&nbsp;   #{(formatTime defaultTimeLocale "%d/%m/%Y às %T" (saidaHrsaida ticket))}
                <p>Valor total: R$ #{saidaVltotal ticket}
        |]