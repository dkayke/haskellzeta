{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Saida where

import Import
import Data.Time.Clock
import Data.Time.LocalTime
import LayoutPark
import System.IO.Unsafe

getSaidaLiR :: Handler Html
getSaidaLiR = do 
        [whamlet|
            <nav>
                <div class="breadcrumb">
                    <div class="col s12 mdl-color--grey-100">
                        <span>HaskPark /
                        <a>Saída

            <label> Pesquisar por placa
            <input type="text" id="searchInput" onkeyup="searchInTable()">

            <table class="mdl-data-table mdl-js-data-table mdl-shadow--2dp" id="searchTable"> 
                <thead>
                    <tr>
                        <th class="mdl-data-table__cell--non-numeric"">Placa do veículo
                        <th class="mdl-data-table__cell--non-numeric">Motorista
                        <th class="mdl-data-table__cell--non-numeric">CNH
                        <th class="mdl-data-table__cell--non-numeric">Ação
                <tbody>
                    <td> teste
                    <td> teste
                    <td> teste
                    <td> teste
                   <button class="mdl-button mdl-js-button mdl-button--raised bt-acao">Saída
        |]

postSaidaDeR :: Handler Html
postSaidaDeR = undefined