{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.CadVeiculo where

import Import
import LayoutPark
import Database.Persist.Postgresql

formVeiculo :: Form (Veiculo, Int)
formVeiculo = renderDivs $ (,) <$> (Veiculo 
    <$> areq (selectField $ optionsPersistKey [] [] tipoveiculoNome) "Tipo Veiculo " Nothing
    <*> areq textField "Placa " Nothing
    <*> areq textField "UF - Cidade da Placa " Nothing
    <*> pure (toSqlKey 0)
    <*> aopt textField "Modelo do Veiculo" Nothing
    <*> aopt intField "Ano " Nothing
    <*> aopt textField "Cor " Nothing)
    <*> areq intField "CNH do Motorista" Nothing
    
getCadVeiculoR :: Handler Html
getCadVeiculoR = undefined

postCadVeiculoR :: Handler Html
postCadVeiculoR = undefined
    