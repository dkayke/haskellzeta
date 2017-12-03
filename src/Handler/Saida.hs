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

cliById :: ClienteId -> Widget
cliById cid = do 
    cli <- handlerToWidget $ runDB $ get404 cid
    [whamlet|
        <td class="mdl-data-table__cell--non-numeric">
            #{clienteNome cli}
        <td class="mdl-data-table__cell--non-numeric">    
            #{clienteCnh cli}
    |]

veicById :: VeiculoId -> Widget
veicById vid = do 
    veic <- handlerToWidget $ runDB $ get404 vid
    [whamlet|
        <td class="mdl-data-table__cell--non-numeric">
            #{veiculoPlaca veic}
    |]
    
getSaidaLiR :: Handler Html
getSaidaLiR = do 
    locacoes <- runDB $ selectList [] [] :: Handler [Entity Entrada]
    layoutPark $ do 
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
                    $forall (Entity locsid entrada) <- locacoes
                        <tr>
                            ^{veicById $ entradaVeiculoid entrada}
                            ^{cliById $ entradaClienteid entrada}
                            <td class="mdl-data-table__cell--non-numeric">
                                <form action=@{SaidaDeR locsid} method=post>
                                    <button class="mdl-button mdl-js-button mdl-button--raised bt-acao">Saída
        |]


utcToMinutes :: UTCTime -> UTCTime -> NominalDiffTime
utcToMinutes a b =  (diffUTCTime  b a) / 60

trunc :: Double -> Int -> Double
trunc x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n

valor :: Double -> Double -> Double -> NominalDiffTime -> Double
valor vlMeia vlHora vlDemais minutos
    | round (minutos) <= 30 && minutos > 0 = trunc vlMeia 2
    | round (minutos) <= 60 && minutos >= 31 = trunc vlHora 2
    | round (minutos) > 60 = trunc (realToFrac ((minutos / 60) * (realToFrac vlDemais :: NominalDiffTime) + (realToFrac vlHora :: NominalDiffTime)) :: Double) 2
    | otherwise = 0

postSaidaDeR :: EntradaId -> Handler Html 
postSaidaDeR locsid = do
    hrsaida  <- liftIO getCurrentTime
    Just negocio <- runDB $ selectFirst [][]
    entrada <- runDB $ selectFirst [EntradaId ==. locsid] []
    case entrada of 
        Nothing -> do 
            setMessage $ [shamlet| Usuario e/ou senha invalido. |]
            redirect LoginR 
        Just (Entity entrid (Entrada cliid veiid hrentr)) -> do
            _ <- runDB $ insert $ Saida (cliid) 
                                        (veiid) 
                                        (hrentr) 
                                        (hrsaida) 
                                        ( valor 
                                            (negocioVlmeiahora . entityVal $ negocio)
                                            (negocioVlhora . entityVal $ negocio)
                                            (negocioVldemais . entityVal $ negocio)
                                            (utcToMinutes hrentr hrsaida)
                                        )
            runDB $ delete locsid
            redirect SaidaLiR