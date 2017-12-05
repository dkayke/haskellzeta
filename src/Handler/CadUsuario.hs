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
getCadUsuarioR = do 
    (widget,enctype) <- generateFormPost formUsuario
    msg <- getMessage
    layoutPark $ do 
        toWidget [julius|
            var p = document.getElementById('hident4');
            var x = 0;
            function show(){
                p.setAttribute('type','text');  
            }
            
            function hide(){
                p.setAttribute('type','password');   
            }
            
            function showHide(){
                document.getElementById("eye").addEventListener("click", function() {
                    if (x == 0){ 
                        show(); 
                        x = 1; 
                    } 
                    else { 
                        hide();
                        x = 0;
                    }
                }, false);
            }
        |]
        
        [whamlet|
            $maybe mensagem <- msg 
                <div id="snackbar"> #{mensagem}
                <script> myFunction();
                
            <nav>
                <div class="breadcrumb">
                    <div class="col s12 mdl-color--grey-100">
                        <span>HaskPark /
                        <a>Cadastro de usuário

            <form action=@{CadUsuarioR} enctype=#{enctype} method=post>
                ^{widget}
                    <i class="material-icons" onclick="showHide()" id="eye">&#xE417;
                    <button class="mdl-button mdl-js-button mdl-button--raised bt-acao">Cadastrar usuário
        |]


postCadUsuarioR :: Handler Html
postCadUsuarioR = undefined