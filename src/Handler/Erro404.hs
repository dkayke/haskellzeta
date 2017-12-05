{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Erro404 where

import Import
import LayoutPark
import Text.Lucius

getErro404R :: Handler Html
getErro404R = defaultLayout $ do
        toWidget [lucius|
            body {
                margin: 0px
            }
            .container404 {
                width: 100vw;
                height: 95vh;
                background: #fff;
                display: flex;
                flex-direction: row;
                justify-content: center;
                align-items: center
            }
            .box404 {
                position:absolute;
                max-width: 710px;
                min-width: 355px;
                max-height: 800px;
                min-height: 400px;
                background-image: url(@{StaticR img_erro404_jpg}); 
                -webkit-background-size: cover;
                -moz-background-size: cover;
                -o-background-size: cover;
                background-size: cover;
            }
            .return404{
                position:absolute; 
                bottom:-0.5em;
                left: 120px;
                text-align: left;
                width:710px;
                font-size:15px;
                font-family: arial;
            }
            .a404{
        		text-decoration: none;
                color: #9914ab;
                outline: none;
                border: none;
                cursor: pointer;
        	}
        |]
        [whamlet|
            <div class="container404">
                <div class="box404">
                    <form action=@{LoginR} method=post>
                        <span class="return404">Volte à sua vaga <a class="a404" href=@{IndexR}>aqui 
        |]