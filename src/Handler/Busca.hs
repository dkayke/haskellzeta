{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Busca where

import Import
import LayoutPark

data Pesq = Pesq 
    {
        dadopesq :: Text
    }
    deriving Show

formBusca :: Form Pesq
formBusca = renderDivs $ Pesq
    <$> areq textField "CNH ou placa do veículo" Nothing