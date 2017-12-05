
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Entrada where

import Import
import LayoutPark

formEntrada :: Form (TipoentradaId, ClienteId, VeiculoId)
formEntrada = renderDivs $ (, , )
    <$> areq (selectField $ optionsPersistKey [] [] tipoentradaNome) "Selecione o tipo de entrada:" Nothing
    <*> areq (selectField $ optionsPersistKey [] [] clienteCnh) "CNH" Nothing
    <*> areq (selectField $ optionsPersistKey [] [] veiculoPlaca) "Placa do Veículo" Nothing

getEntradaR :: Handler Html
getEntradaR = undefined

postEntradaR :: Handler Html
postEntradaR = undefined
